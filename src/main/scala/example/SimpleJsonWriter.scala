package example

import sttp.client3._

import io.circe._
import io.circe.parser._
import cats.instances.list
import scala.annotation.tailrec

import cats.effect.{IO, IOApp} 
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import java.nio.Buffer
import cats.effect.kernel.Resource

import cats.implicits._ // for parSequence_ 

object SimpleJsonWriter { 

    private val content: String = fetch()
    private val listOfJsons0: List[String] = content.substring(1, content.length() - 1).trim().split("},").toList.map(s => s + "}")
    private val listOfJsons: List[String] = modifyLast(listOfJsons0)(s => s.substring(0, s.length() - 1))

    private val sequenceOfJsonWriters = listOfJsons.map(jsonContent => writeJsonToFile(jsonContent))

    def start(): IO[Unit] = sequenceOfJsonWriters.parSequence_

    private def fetch(): String = { 
        val request = basicRequest.get(uri"https://jsonplaceholder.typicode.com/posts")
        val backend = HttpURLConnectionBackend()
        val response = request.send(backend)
        val content: String = response.body.getOrElse("")

        content
    }

    private def modifyLast[A](xs: List[A])(f: A => A): List[A] = { 

        @tailrec
        def modifyLastTailRec(remainingList: List[A], newList: List[A], currentIndex: Int): List[A] = { 
            if (currentIndex == xs.length - 1) f(remainingList.head) :: newList
            else modifyLastTailRec(remainingList.tail, remainingList.head :: newList, currentIndex + 1)
        }

        modifyLastTailRec(xs, Nil, 0).reverse
    }

    private def openFileWriter(path: String): IO[BufferedWriter] = { 
        IO(println(s"[${Thread.currentThread().getName}] opening $path")) >> 
        IO(new BufferedWriter(new FileWriter(new File(path))))
    }

    private def writeResourceToFile(path: String) = 
        Resource.make(openFileWriter(path)) { bw => 
            IO(println(s"[${Thread.currentThread().getName}] closing $path")) >>
            IO(bw.close())
        }

    private def writeJsonToFile(jsonContent: String): IO[Unit] = { 
        val doc: Json = parse(jsonContent).getOrElse(Json.Null)
        val cursor: HCursor = doc.hcursor
        val fileId = cursor.get[Int]("id").getOrElse(-17).toString()

        writeResourceToFile("output/" + fileId).use { 
            bw => { 
                IO(println(s"[${Thread.currentThread().getName}] writing to output/$fileId")) >>
                IO(bw.write(jsonContent.trim()))
            }
       }
    }








}