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
 * Dieses Beispiel wurde aus einem Peer-to-Peer-Programm portiert. Ziel dieser Klasse ist es, Dateitransfer-Meldungen zu verarbeiten.
 * Eine Dateitransfer-Meldung ist entweder 
 * * Ein FileChunkRequest zum Anfordern eines Teils der Datei
 * * Ein FileChunkResponse als Antwort auf einen FileChunkRequest
 * * Ein FileHeaderRequest zum initiieren eines Dateitransfers
 * * Ein FileHeaderResponse mit allen Chunks der Datei
 * 
 * Der Ablauf der Dateiübertragung ist wie folgt:
 * 
 * Client			Server
 * --------------------------
 * FileHeaderRequest ->
 * 				<-	FileHeaderResponse(FileTokens : Array[long])
 * FileChunkReqeust	 ->
 * 				<-	FileChunkResponse(data : Array[byte])
 * FileChunkReqeust	 ->
 * 				<-	FileChunkResponse(data : Array[byte])
 * ....
 * FileChunkReqeust	 ->
 * 				<-	FileChunkResponse(data : Array[byte])
 * 
 * Die Dateiübertragung ist abgeschlossen, wenn alle Chunks beim Client angekommen sind.
 * 
 */
object FileMessageKind extends Enumeration {
	type FileMessageKind = Value
	val FileChunk,FileHeader = Value
}

import FileMessageKind._

class FileMessage(id : UUID, payload : String, remoteHost : Servant, kind : FileMessageKind) {
	val Id = id
	val Payload = payload
	val RemoteHost = remoteHost
	val Kind = kind
}

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
	val receive = new ImperativeEvent[FileMessage] 
	val send = new ImperativeEvent[FileMessage]
	
	val fileTransferStopped = new ImperativeEvent[Unit]
	                                              
	val missingChunks = new ListBuffer[Long]
	
	val fileHeaderReceived = receive && ((msg : FileMessage) => msg.Kind == FileMessageKind.FileHeader ) 
	val transferPending = new BetweenEvent(fileHeaderReceived, fileTransferStopped)
	val fileChunkReceived = receive && ((msg:FileMessage) => msg.Kind  == FileMessageKind.FileChunk) within transferPending
	
	transferPending.before += startTransfer
	
	/*
	 * The grammar of the file Header is <filename>(;<chunk_id>)*
	 * where filename is a string and
	 * chunk_id is a long value
	 */
	def startTransfer(msg : FileMessage) {
		val chunkIdList = msg.Payload.split(",")
		
		chunkIdList.drop(1).foreach( (s)=> missingChunks.append(s.toLong))
		
		requestNextChunk(msg.RemoteHost, chunkIdList.first )
	}
	
	def requestNextChunk(receiverHost : Servant, file : String) {
		
		if(missingChunks.length == 0) {
			fileTransferStopped()
			return
		}
		val chunkId = missingChunks.first
		val request = new FileMessage(UUID.randomUUID(), chunkId.toString, this, FileMessageKind.FileChunk)
		val requestPending = new BetweenEvent(send, receive && ((msg : FileMessage) => msg.Id  == request.Id ))
		requestPending.after += ((msg : FileMessage) => {
			storeChunkData(chunkId, msg, file)
			requestNextChunk(msg.RemoteHost, file)
		})
		
		send(receiverHost, request)
	}
	
	def storeChunkData(chunkId : Long, msg : FileMessage, fileName : String) {
		
		print (fileName + ": received chunk " + chunkId)
		//Todo: Store the file chunk on disk
		missingChunks .remove(0)
	}
	
	def downloadFile(from : Servant, file : String) {
		val request = new FileMessage(UUID.randomUUID(), file, this, FileMessageKind.FileHeader )
		send(from, request)
	}
		
	override def send(to: Servant, msg : FileMessage) {
		super.send(to, msg)
		send(msg)
	}
	
	override def receive(msg : FileMessage, from : Servant) {
		super.receive(msg, from)
		receive (msg)
	}
}

class StateDrivenFileTransfer extends Servant{
	object TransferState extends Enumeration {
		type TransferState = Value
		val WaitForHeader, Pending, Finished = Value
	}
	
	var state = TransferState.WaitForHeader 
	val missingChunks = new ListBuffer[Long]
	var fileName : String = null
	
	
	private def handleFileHeader(msg : FileMessage) {
		if(state != TransferState.WaitForHeader )
		{
			print("Received FileHeader in State " + state)
			return
		}
		
		val tokens = msg.Payload .split(",")
		tokens.drop(1).foreach(s=>missingChunks.append(s.toLong))
		fileName = tokens.first
		
		state = TransferState.Pending
	}
	
	var chunkMap = Map[UUID, Long]()
	
	def downloadFile(from : Servant, file : String) {
		val request = new FileMessage(UUID.randomUUID(), file, this, FileMessageKind.FileHeader )
		send(from, request)
	}
	
	private def handleFileChunk(msg : FileMessage) {
		if(state != TransferState.Pending ) {
			print("There is currently no transfer pending!")
			return
		}
		
		//Todo: Find the corresponding request and the chunk id
		val chunkId = chunkMap(msg.Id)
		
		print(fileName + ": Received Chunk " + chunkId)
		
		if(missingChunks.length == 0)
		{
			state = TransferState.Finished
			print("File transfer finished")
		}
		else {
			requestChunk(msg.RemoteHost )
		}
	}
	
	def requestChunk(from : Servant) {
		val chunkId = missingChunks .first
		val request = new FileMessage(UUID.randomUUID(), chunkId.toString, this, FileMessageKind.FileChunk)
		chunkMap(request.Id) = chunkId
		send(from, request)
	}
	
	override def receive(msg : FileMessage, from : Servant) {
		super.receive(msg, from)
		
		if(msg.Kind == FileMessageKind.FileHeader ) {
			handleFileHeader(msg);
		}
		else if(msg.Kind  == FileMessageKind.FileChunk ) {
			handleFileChunk(msg)
		}
	}
}

object ChordFileTransferRun {
	def main(args : Array[String]) : Unit = {
		
	}
}