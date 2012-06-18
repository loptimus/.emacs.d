package refactorerl.ui.core.inboxlisteners;

import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import refactorerl.ui.core.types.MessageType;
import refactorerl.ui.core.types.QueryResult;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

class ReplyHandler extends RequestHandlerAdapter {
	private static final Logger logger = Logger.getLogger(ReplyHandler.class.toString());
	
	public ReplyHandler(RequestListener requestListener) {
		this.requestListener = requestListener;
	}
	
	public Map<MessageType, Object> parseMessage(OtpErlangTuple message) {
		Map<MessageType, Object> resultMap = new HashMap<MessageType, Object>();
		
		if (message.elementAt(1).toString().equals("reply")) {
			logger.info(message.elementAt(2).toString());
			if (message.elementAt(2) instanceof OtpErlangTuple) {
				OtpErlangTuple reply = (OtpErlangTuple)message.elementAt(2);
				
				if (reply.arity() > 1 && reply.elementAt(0).toString().equals("ok")) {
					OtpErlangObject result = reply.elementAt(1);
					
					if (result instanceof OtpErlangList) {
						for (OtpErlangObject obj : (OtpErlangList)result) {
							resultMap.put(MessageType.other, parseMessageElement(obj));
						}
					} else if (result instanceof OtpErlangTuple) {
						OtpErlangTuple toProcess = (OtpErlangTuple)result;
						String key = toProcess.elementAt(0).toString();
						
						if ( key.equals("result") &&
							 toProcess.elementAt(1) instanceof OtpErlangList) {
							for (OtpErlangObject obj : (OtpErlangList)toProcess.elementAt(1)) {
								OtpErlangTuple resultMsg = (OtpErlangTuple)obj;
								resultMap.put(
										MessageType.valueOf(resultMsg.elementAt(0).toString()),
										parseMessageElement(resultMsg.elementAt(1)));
							}
						}
						else if (key.equals("abort")) {
							//OtpErlangTuple msg = (OtpErlangTuple)toProcess.elementAt(1);
							//throw new ErlangException(((OtpErlangString)msg.elementAt(1)).stringValue());
						} else {
							resultMap.put(
									MessageType.other,
									parseMessageElement(toProcess));
						}
					} else {
						resultMap.put(
								MessageType.other,
								parseMessageElement(reply));
					} 
				} else if (reply.arity() > 1 && reply.elementAt(0).toString().equals("error")) {
					// TODO: handle error
					logger.info(reply.toString());
				}
			}
		}
		
		return Collections.unmodifiableMap(resultMap);
	}
	
	private static Object parseMessageElement(OtpErlangObject message) {
		if (message instanceof OtpErlangList) {
			ArrayList<Object> msgList = new ArrayList<Object>();
//{group_by,{{"asd.erl",256,298},"asd:korte/1"},eq,name,korte},
//{group_by,{nopos,"refusr_strm.erl"},list,[{{"refusr_strm.erl",1838,2056},"refusr_strm:getDistance/2"}]
			for (OtpErlangObject element : (OtpErlangList)message) {
				if (element instanceof OtpErlangTuple) {
					OtpErlangTuple tuple = (OtpErlangTuple)element;
					
					if (tuple.elementAt(0).toString().equals("group_by")) {
						QueryResult result = parseResult(tuple.elementAt(1));
						
						if (tuple.elementAt(2).toString().equals("list")) {
							for (OtpErlangObject info : (OtpErlangList)tuple.elementAt(3)) {
								result.add(parseResult(info));
							}
						} else if (tuple.elementAt(2).toString().equals("eq")) {
							String eq = tuple.elementAt(3).toString() + " = " + tuple.elementAt(4).toString();
							result.add(new QueryResult(eq));
						}
						
						msgList.add(result);
					} else if (tuple.elementAt(0).toString().equals("list")) {
						for (OtpErlangObject info : (OtpErlangList)tuple.elementAt(1)) {
							logger.info(info.toString());
							msgList.add(parseResult(info));
						}
					} else {
						msgList.add(tuple.toString());
					}
				} else {
					msgList.add(element.toString());
				}
			}
			
			return Collections.unmodifiableList(msgList);
		} else {
			return message.toString();
		}
	}
	
	private static QueryResult parseResult(OtpErlangObject info) {
		if (info instanceof OtpErlangTuple) {
			OtpErlangTuple elem = (OtpErlangTuple)info;
			String nodeValue = ((OtpErlangString)elem.elementAt(1)).stringValue();
			
			if (elem.elementAt(0) instanceof OtpErlangTuple) {
				OtpErlangTuple file = (OtpErlangTuple)elem.elementAt(0);
				String fileAbsPath = ((OtpErlangString)file.elementAt(0)).stringValue();
				int startPos = Integer.parseInt(file.elementAt(1).toString());
				int endPos = Integer.parseInt(file.elementAt(2).toString());
				
				return new QueryResult(fileAbsPath, startPos, endPos, nodeValue);
			} else {
				return new QueryResult(nodeValue);
			}
		} else {
			return new QueryResult(info.toString());
		}
	}
	
	public OtpErlangObject handle(OtpErlangTuple message) {
		String key = message.elementAt(1).toString();
		logger.info(message.toString());
		
		if (key.equals("reply")) {
			if (message.elementAt(2) instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple) message.elementAt(2);
				
				//A reply can be a success(ok) or a fail(error)
				if( tuple.arity() > 1 &&
					tuple.elementAt(0).toString().equals("ok"))
				{
					if(tuple.elementAt(1) instanceof OtpErlangTuple) {
						handleOk((OtpErlangTuple)tuple.elementAt(1));
					}
				}
				else if ( tuple.arity() > 1 && 
						  tuple.elementAt(0).toString().equals("error")) 
				{
					logger.info("Error Occured! Removing: " + tuple);
				}
				return tuple;
			}
		}
		
		return null;
	}
	
	private void handleOk(OtpErlangTuple tuple) {
		if( tuple.arity() == 2 && 
			tuple.elementAt(0).toString().equals("result") &&
			tuple.elementAt(1) instanceof OtpErlangList)  {
			OtpErlangList list = (OtpErlangList) tuple.elementAt(1);
			
			for (OtpErlangObject o : list) {
				if (o instanceof OtpErlangTuple) {
					OtpErlangTuple toProcess = (OtpErlangTuple) o;
					String key = toProcess.elementAt(0).toString();
					
					logger.info(toProcess.toString());
					
					if( !(toProcess.elementAt(1) instanceof OtpErlangList) ) {
						continue;
					}
					
					OtpErlangList results = (OtpErlangList)toProcess.elementAt(1);
					
					if (key.equals("result")) {
						processResult(results);
					} else if (key.equals("saved")) {
						//TODO implement
						processSaved(results);
					} else if (key.equals("renamed")) {
						//TODO implement
						processRenamed(results);
					}
				}
			}
		}
	}
	
	private void processResult(OtpErlangList results) {
		for(OtpErlangObject o : results) {
			logger.info(o.toString());
		}
	}
	
	private void processSaved(OtpErlangList results) {
		//TODO not yet implemented
	}
	
	private void processRenamed(OtpErlangList results) {
		//TODO not yet implemented
	}
}
