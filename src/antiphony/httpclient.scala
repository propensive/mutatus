package antiphony

import quarantine._
import euphemism._

import javax.servlet._, http._

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.annotation.tailrec

import java.net._
import java.io._

sealed abstract class HttpException(url: String, code: Int) extends Exception

case class NotFound(url: String) extends HttpException(url, 404)
case class NotAuthorized(url: String) extends HttpException(url, 401)
case class OtherException(url: String, code: Int) extends HttpException(url, code)

object Postable {
  implicit object string extends Postable[String]("text/plain") {
    def content(value: String): Array[Byte] = value.getBytes("UTF-8")
  }

  implicit object map extends Postable[Map[String, String]]("multipart/form-data") {
    def content(value: Map[String, String]): Array[Byte] =
      value.map { case (key, value) =>
        s"${URLEncoder.encode(key, "UTF-8")}=${URLEncoder.encode(value, "UTF-8")}"
      }.mkString("&").getBytes("UTF-8")
  }

  implicit object json extends Postable[Json]("application/json") {
    def content(value: Json): Array[Byte] = value.toString.getBytes("UTF-8")
  }
}

abstract class Postable[T](val contentType: String) { def content(value: T): Array[Byte] }

case class HttpHeader(key: String, value: String)

object Http extends Domain[HttpException] {

  def post[T: Postable](url: String, content: T, headers: Set[HttpHeader]): Result[Array[Byte]] =
    request[T](url, content, "POST", headers)

  def get(url: String, headers: Set[HttpHeader]): Result[Array[Byte]] =
    request(url, Map[String, String](), "GET", headers)

  def request[T: Postable](url: String, content: T, method: String, headers: Set[HttpHeader]): Result[Array[Byte]] = {
    new URL(url).openConnection match {
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method)
        conn.setRequestProperty("Content-Type", implicitly[Postable[T]].contentType)
        conn.setRequestProperty("User-Agent", "Furore 1.0.0")
        headers.foreach { case HttpHeader(key, value) => conn.setRequestProperty(key, value) }
        
        if(method == "POST") {
          conn.setDoOutput(true)
          val out = conn.getOutputStream()
          out.write(implicitly[Postable[T]].content(content))
          out.close()
        }

        conn.getResponseCode match {
          case 200 =>
            val in = conn.getInputStream()
            val data = new ByteArrayOutputStream()
            val buf = new Array[Byte](65536)
            
            @tailrec
            def read(): Array[Byte] = {
              val bytes = in.read(buf, 0, buf.length)
              if(bytes < 0) data.toByteArray else {
                data.write(buf, 0, bytes)
                read()
              }
            }
            Result(read())
          case 404 => Error(NotFound(url))
          case 401 => Error(NotAuthorized(url))
          case code => Error(OtherException(url, code))
        }
    }
  }
}
