package examples.filetransfer

import scala.util.Random
import scala.collection.mutable.HashMap
import scala.events.Event
import scala.collection.mutable.ListBuffer
import scala.events.BetweenEvent
import scala.events.ImperativeEvent
import java.net.InetAddress
import java.util.UUID 
/***
 * 
 * @author Frank Englert
 *
 * This example was ported from a java p2p application. The application shares files between
 * differen hosts in a computer network. The filetransfer-component takes file messages and 
 * writes them to disk. 
 * 
 * The Communication looks like the following steps
 * 
 * Client			Server
 * --------------------------
 * FileHeaderRequest(data: fileId) ->
 * 				<-	FileHeaderResponse(data : <chunkid1>,<chunkid2>,...)
 * FileChunkReqeust	 ->
 * 				<-	FileChunkResponse(data : <dta>)
 * FileChunkReqeust(data: fileChunkId)	 ->
 * 				<-	FileChunkResponse(data : file content)
 * ....
 * FileChunkReqeust	 ->
 * 				<-	FileChunkResponse(data : Array[byte])
 * 
 * The transfer is finished, when all chunks are received by the client.
 * 
 * 
 */
object FileMessageKind extends Enumeration {
	type FileMessageKind = Value
	val FileChunk,FileHeader = Value
}

import FileMessageKind._

/**
 * A file message can either be:
 * * a FileChunkRequest to request an arbitrary file token
 * * a FileChunkResponse as response for a file chunk request
 * * a FileHeaderRequest to initiate a file transfer
 * * a FileHeaderResponse a response of the FileHeaderRequest containing all file chunks that are available.
 * 
 * @author Frank Englert
 *
 */
class FileMessage(id : UUID, payload : String, remoteHost : Servant, kind : FileMessageKind) {
	val Id = id
	val Payload = payload
	val RemoteHost = remoteHost
	val Kind = kind
}

/**
 * The Servant provides an interface to interconnet the components of the application. In real live the 
 * meesages to send or to receive would be serialized and streamed over a network connection to a foreign host.
 * But in this example the file server and the file host are members of the same process. So we can connect the 
 * components directly without transferring the messages over the net.
 * 
 * @author Frank Englert
 *
 */
trait Servant {
	def receive(msg : FileMessage, from : Servant) {
		
	}
	
	def send(to: Servant, msg : FileMessage) {
		to.receive(msg, this);
	}
}

trait FileClient {
	def downloadFile(from : Servant, name : String)
}

class EventDrivenFileTransfer extends Servant with FileClient{
	val onReceive = new ImperativeEvent[FileMessage] 
	val send = new ImperativeEvent[FileMessage]
	
	val fileTransferStopped = new ImperativeEvent[Unit]
	                                              
	val missingChunks = new ListBuffer[Long]
	
	val fileHeaderReceived = onReceive && ((msg : FileMessage) => msg.Kind == FileMessageKind.FileHeader ) 
	val transferPending = new BetweenEvent(fileHeaderReceived, fileTransferStopped)
	val fileChunkReceived = onReceive && ((msg:FileMessage) => msg.Kind  == FileMessageKind.FileChunk) within transferPending
	
	transferPending.before += startTransfer
	transferPending.after += ((Unit) => println("File transfer finished"))
	
	/*
	 * The grammar of the file Header is <filename>(;<chunk_id>)*
	 * where filename is a string and
	 * chunk_id is a long value
	 */
	def startTransfer(msg : FileMessage) {
		
		val header = msg.Payload.split(":")
		val chunkIdList = header(1).split(",")
		println("Header is " + msg.Payload )
		chunkIdList.foreach( (s)=> missingChunks.append(s.toLong))
		requestNextChunk(msg.RemoteHost, header(0))
	}
	
	def requestNextChunk(receiverHost : Servant, file : String) {
		
		if(missingChunks.length == 0) {
			fileTransferStopped()
			return
		}
		val chunkId = missingChunks.first
		val request = new FileMessage(UUID.randomUUID(), chunkId.toString, this, FileMessageKind.FileChunk)
		val requestPending = new BetweenEvent(send, onReceive && ((msg : FileMessage) => {msg.Id.equals(request.Id)} ))
		lazy val onResponseReceive : (FileMessage) => Unit = ((msg : FileMessage) => {
			requestPending.after -= onResponseReceive
			println("Received request result")
			storeChunkData(chunkId, msg, file)
			requestNextChunk(msg.RemoteHost, file)
		});
		requestPending.after += onResponseReceive
		send(receiverHost, request)
	}
	
	def storeChunkData(chunkId : Long, msg : FileMessage, fileName : String) {
		
		println (fileName + ": received chunk. Number of missing chunks " + missingChunks.size)
		//Todo: Store the file chunk on disk
		missingChunks .remove(0)
	}
	
	def downloadFile(from : Servant, file : String) {
		val request = new FileMessage(UUID.randomUUID(), file.hashCode.toString, this, FileMessageKind.FileHeader )
		send(from, request)
	}
		
	override def send(to: Servant, msg : FileMessage) {
		send(msg)
		super.send(to, msg)
	}
	
	override def receive(msg : FileMessage, from : Servant) {
		super.receive(msg, from)
		onReceive (msg)
	}
}

class StateDrivenFileTransfer extends Servant{
	object TransferState extends Enumeration {
		type TransferState = Value
		val WaitForHeader, Pending, Finished = Value
	}
	
	var state = TransferState.WaitForHeader 
	val missingChunks = new ListBuffer[Int]
	var fileName : String = null
	
	
	private def handleFileHeader(msg : FileMessage) {
		if(state != TransferState.WaitForHeader )
		{
			println("Received FileHeader in State " + state)
			return
		}
		val header = msg.Payload.split(":")
		
		val tokens = header(1).split(",")
		tokens.foreach(s=>missingChunks.append(s.toInt))
		fileName = header(0)
		
		state = TransferState.Pending
	}
	
	var chunkMap = HashMap[UUID, Int]()
	
	def downloadFile(from : Servant, file : String) {
		val request = new FileMessage(UUID.randomUUID(), file.hashCode.toString, this, FileMessageKind.FileHeader )
		send(from, request)
	}
	
	private def handleFileChunk(msg : FileMessage) {
		if(state != TransferState.Pending ) {
			println("There is currently no transfer pending!")
			return
		}
		
		//Todo: Find the corresponding request and the chunk id
		val chunkId = chunkMap(msg.Id)
		
		println(fileName + ": Received Chunk. Number of missing Chunks is " + missingChunks.length)
		missingChunks -= chunkId
		if(missingChunks.length == 0)
		{
			state = TransferState.Finished
			println("File transfer finished")
		}
	}
	
	def requestChunk(from : Servant) {
		
		if(state != TransferState.Pending )
			return
		val chunkId = missingChunks .first
		val request = new FileMessage(UUID.randomUUID(), chunkId.toString, this, FileMessageKind.FileChunk)
		
		chunkMap.put(request.Id, chunkId)
		
		send(from, request)
	}
	
	override def receive(msg : FileMessage, from : Servant) {
		super.receive(msg, from)
		
		if(msg.Kind == FileMessageKind.FileHeader ) {
			println("Received file header. ")
			handleFileHeader(msg);
			requestChunk(from)
		}
		else if(msg.Kind  == FileMessageKind.FileChunk ) {
			handleFileChunk(msg)
			requestChunk(from)
		}
	}
}

class TestFileServer extends Servant {
	
	val bucket = HashMap[Int, String]()
	
	def add(name : String, content : String ) {
		val chunkSize = 128
		
		val chunks = new ListBuffer[Int]
		//1. Step: Split content into chunks
		for(offset <- 0 until content.size by chunkSize) {
			val data = content.substring(offset, Math.min(offset+chunkSize, content.size))
			chunks.append(data.hashCode())
			//2. Add Chunks to the bucket
			bucket.put(data.hashCode, data)
		}

		//3. Add the FileHeader to the bucket
		val header = chunks.foldLeft(name+":")((x,y)=>x+y.toString()+",")
		//println("Header is " + header)
		bucket.put(name.hashCode, header)
	}
	
	override def receive(msg: FileMessage, from : Servant) {
		println("FileServer received request")
		val selectedKey: Int= msg.Payload.toInt
		
		if(!bucket.contains(selectedKey))
			println("The requested key  " + selectedKey + " does not exist")
		
		val reply = new FileMessage(msg.Id, bucket.get(selectedKey).getOrElse(""), this, msg.Kind)
		
		this.send(from, reply)
	}
}

object Run {
	def main(args : Array[String]) : Unit = {
		val server = new TestFileServer()
		val stateDrivenFileTransfer = new StateDrivenFileTransfer
		val eventDrivenFileTransfer = new EventDrivenFileTransfer
		
		val rnd = new Random()
		server.add("file01.txt", rnd.nextString(1023))
		
		println("Added file01.txt")
		
		println("Starting State driven Filetransfer")
		stateDrivenFileTransfer.downloadFile(server, "file01.txt")
		
		println("Starting Event driven Filetransfer")
		eventDrivenFileTransfer.downloadFile(server, "file01.txt")
	}
}