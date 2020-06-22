
package antiphony

import quarantine._
import euphemism._

import javax.servlet._, http._

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.annotation.tailrec

import java.net._
import java.io._
import HttpServerHelpers._

case class ServletResponseWriter(r: HttpServletResponse) extends ResponseWriter {

  def setStatus(status: Int) = {
    r.setStatus(status)
  }

  def writeBytes(bytes: Array[Byte]) = {
    r.getOutputStream().write(bytes)
  }

  def appendBody(body: String) = {
    r.getWriter().println(body)
  }

  def setContentType(contentType: String) = {
    r.setContentType(contentType)
  }

  def addHeader(key: String, value: String) = {
    r.addHeader(key, value)
  }

  def sendRedirect(url: String) = {
    r.sendRedirect(url)
  }

  def close = {}
}

abstract class ServletWrapper() extends HttpServlet with RequestHandler {

  override def doGet(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse
  ): Unit = handleRequest(servletRequest, servletResponse)

  override def doPost(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse
  ): Unit = handleRequest(servletRequest, servletResponse)

  override def doPut(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse
  ): Unit = handleRequest(servletRequest, servletResponse)

  override def doDelete(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse
  ): Unit = handleRequest(servletRequest, servletResponse)

  override def doOptions(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse
  ): Unit = handleRequest(servletRequest, servletResponse)
  
  def handleRequest(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse
  ) = {
    val responseWriter = ServletResponseWriter(servletResponse)
    handle(mapRequest(servletRequest)).respond(responseWriter)
  }

  private def mapRequest(request: HttpServletRequest): Request = {
    val in = request.getInputStream
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

    val content = read()
    val requestHeaders = request.getHeaderNames.asScala.toList.map { k => k -> request.getHeader(k) }.toMap
    val requestParams = request.getParameterNames.asScala.toList.map { k => k -> request.getParameterValues(k).toList }.toMap
    Request(Method.from(request.getMethod),
      request.getContentType,
      request.getContentLength,
      content,
      request.getQueryString,
      request.isSecure,
      request.getServerName,
      request.getServerPort,
      request.getRequestURI,
      mapKeysToLowerCase(requestHeaders),
      mapKeysToLowerCase(requestParams))
  }

}
