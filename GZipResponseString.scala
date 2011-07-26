package net.gerdriesselmann.unfiltered

import unfiltered.request.{HttpRequest, AcceptEncoding}
import unfiltered.response._

/**
 * GZip a response, if possible
 */
case class GZipResponseString[T](request: HttpRequest[T], content: String) extends Responder[Any] {
	/**
	 * Respond
	 */
	override def respond(res: HttpResponse[Any]) {
		request match {
			case AcceptEncoding(values) => if (values.exists { s => s.equalsIgnoreCase("gzip") }) {
				streamGZip(res)
			} else {
				streamRes(res)
			}
			case _ => streamRes(res)
		}
	}

	/**
	 * Turn string into bytes
	 */
	def contentToBytes = content.getBytes("UTF-8")

	/**
	 * Stream using the default output stream
	 */
	def streamRes(res: HttpResponse[Any]) = stream(res.getOutputStream, content)

	/**
	 * Stream using gzip encoding
	 */
	def streamGZip(res: HttpResponse[Any]) = {
		res.addHeader("Content-Encoding", "gzip")
		val gos = new java.util.zip.GZIPOutputStream(res.getOutputStream)
		stream(gos, content)
	}

	/**
	 * Write into given stream
	 */
	def stream(os: java.io.OutputStream, content: String) {
		try { os.write(contentToBytes) }
    	finally { os.close() }
	}
}
