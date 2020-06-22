package antiphony

import java.net.InetSocketAddress;
 
import com.sun.net.httpserver._
import java.io.OutputStream
import java.net.URI
import java.io.ByteArrayOutputStream
import scala.collection.JavaConverters._
import LightweightServer._
import HttpServerHelpers._

object LightweightServer {

  case class RequestQuery(query: String, parameters: Map[String, List[String]]) 

  def readBody(exchange: HttpExchange): Array[Byte] = {
    val is = exchange.getRequestBody()
    val output = new ByteArrayOutputStream();
    val buffer = Array.ofDim[Byte](2048)
    var len = is.read(buffer)
    while(len > 0) {
      println(len)
      output.write(buffer, 0, len)
      is.read(buffer)
    }
    output.toByteArray()
  }

  def readQuery(exchange: HttpExchange): RequestQuery = {
    val uri = exchange.getRequestURI()
    val query = uri.getQuery()
    if(query != null) {
      val paramStrings = query.split("&")
      val params = paramStrings.foldLeft(Map[String, List[String]]()) { (map, elem) =>
        val split = elem.split("=")
        val key = split(0)
        val value = split(1)
        val values = map.getOrElse(key, List.empty[String])
        map + (key -> (value::values))
      }
      RequestQuery(query, params)
    } else {
      RequestQuery(query, Map())
    }
  }

  def readHeaders(exchange: HttpExchange): Map[String, String] = {
    exchange.getRequestHeaders().asScala.mapValues(_.get(0)).toMap
  }

  def mapToRequest(exchange: HttpExchange): Request = {
    val body = readBody(exchange)
    val query = readQuery(exchange)
    val headers = readHeaders(exchange)
    Request(
      Method.from(exchange.getRequestMethod()),
      exchange.getRequestHeaders().getFirst("content-type"),
      body.length,
      body,
      query.query,
      false,
      exchange.getRequestURI().getHost(),
      exchange.getRequestURI().getPort(),
      exchange.getRequestURI().getPath(),
      mapKeysToLowerCase(headers),
      mapKeysToLowerCase(query.parameters)
    )
  }

  case class LightweightServerResponseWriter(exchange: HttpExchange) extends ResponseWriter {

    var status = 200

    def writeBytes(bytes: Array[Byte]): Unit = {
      exchange.sendResponseHeaders(status, bytes.length)
      exchange.getResponseBody().write(bytes)
      exchange.getResponseBody().flush()
    }

    def appendBody(payload: String) = {
      exchange.sendResponseHeaders(status, payload.getBytes().length)
      exchange.getResponseBody().write(payload.getBytes())
      exchange.getResponseBody().flush()
    }

    def setContentType(contentType: String) = {
      exchange.getResponseHeaders().add("content-type", contentType)
    }

    def addHeader(key: String, value: String) = {
      exchange.getResponseHeaders().add(key, value)
    }

    def sendRedirect(url: String) = {
      exchange.sendResponseHeaders(301, 0)
      addHeader("Location", url)
    }

    def setStatus(status: Int) = {
      this.status = status
    }

    def close() = {
      exchange.close()
    }
  }

}

abstract class LightweightServer(port: Int, rootPath: String) extends RequestHandler {

    val httpServer = HttpServer.create(new InetSocketAddress(port), 0)

    val handler: HttpHandler = { (exchange: HttpExchange) =>
      try {
        val request = mapToRequest(exchange)
        println(request)
        val responseWriter = LightweightServerResponseWriter(exchange)
        handle(request).respond(responseWriter)
      } catch {
        case exception: Throwable => exception.printStackTrace()
      }
    }
  
    val context = httpServer.createContext(rootPath)
    context.setHandler(handler)
    httpServer.setExecutor(null)
    httpServer.start()
}

