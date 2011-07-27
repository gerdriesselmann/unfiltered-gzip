package net.gerdriesselmann.unfiltered

import unfiltered.response._
import unfiltered.filter.Plan
import java.io.PrintWriter
import unfiltered.request.{Accepts, HttpRequest, AcceptEncoding}
import unfiltered.Cycle


/**
 * Extractor for Accept-Encoding header
 *
 * Use like:
 *
 * req match {
 *   case AcceptsEncoding.GZip(_) => ....
 * }
 */
object AcceptsEncoding {
	trait AcceptingEncoding {
		val encodingType: String

		def unapply[T](r: HttpRequest[T]) = r match {
			case AcceptEncoding(values) =>
				if (values.exists {s => s.equalsIgnoreCase(encodingType) }) Some(r)
				else None
			}
	}

	object GZip extends AcceptsEncoding.AcceptingEncoding {
		val encodingType = "gzip"
	}
}

/**
 * GZip a response, if possible
 *
 * Taken from here: https://github.com/gerdriesselmann/unfiltered-gzip
 */
case class GZipResponseString[T](request: HttpRequest[T], content: String) extends Responder[Any] {
	/**
	 * Respond
	 */
	override def respond(res: HttpResponse[Any]) {
		request match {
			case AcceptsEncoding.GZip(_) => streamGZip(res)
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

/**
 * HttpResponse that gzips its content
 */
class GZipHttpResponse[T](val source: HttpResponse[T]) extends HttpResponse[T](source.underlying) {
	source.addHeader("Content-Encoding", "gzip")
	val gzipstream = new java.util.zip.GZIPOutputStream(source.getOutputStream())
	val writer = new PrintWriter(gzipstream)

	def setContentType(contentType : scala.Predef.String) : scala.Unit = source.setContentType(contentType)
	def setStatus(statusCode : scala.Int) : scala.Unit = source.setStatus(statusCode)
	def getWriter() : java.io.PrintWriter = writer
	def getOutputStream() : java.io.OutputStream = gzipstream
	def sendRedirect(url : scala.Predef.String) : scala.Unit = source.sendRedirect(url)
	def addHeader(name : scala.Predef.String, value : scala.Predef.String) : scala.Unit = source.addHeader(name, value)
	def cookies(cookie : scala.Seq[unfiltered.Cookie]) : scala.Unit	 = source.cookies(cookie)
}

/**
 * This Intent can be used prior to all other matching partial functions like so:
 *
 * def intent = GZipIntent {
 *   case GET(_) => ...
 * }
 */
object GZipIntent {
	/**
	 * Wrapper around ResponseFunction that switches HttpResponse to GZip, if allowed
	 */
	case class GZipResponseFunctionWrapper[A](req: HttpRequest[A], f: ResponseFunction[Any]) extends ResponseFunction[Any] {
		def apply[T](res: HttpResponse[T]) = req match {
			case AcceptsEncoding.GZip(_) => {
				val gos = new GZipHttpResponse(res)
				f(gos)
			}
			case _ => f(res)
		}
	}

	/**
	 * Factory to wrap original intent
	 */
	def apply[A](inner: Cycle.Intent[A,Any]): Cycle.Intent[A,Any] = {
		case req @ _ => {
			inner.orElse({ case _ => Pass }: Cycle.Intent[A,Any])(req) match {
				case Pass => Pass
				case responseFunction => GZipResponseFunctionWrapper(req, responseFunction)
			}
		}

	}
	
}
