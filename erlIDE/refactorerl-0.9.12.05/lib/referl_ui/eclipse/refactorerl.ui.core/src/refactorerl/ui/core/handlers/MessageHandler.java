package refactorerl.ui.core.handlers;

import java.util.ArrayList;
import java.util.Map;
import java.util.logging.Logger;

import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.connection.IInboxListener;
import refactorerl.ui.core.extensionpoints.ConverterExtensionPointProxy;
import refactorerl.ui.core.extensionpoints.ExtensionPointParser;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * This is the "folder" of incoming messages.
 * All messages received from the RefactorErl tool are converted here
 * and sent to all listeners
 * @author Zolt�n Verebes, modified by Jozsa Bela - Tamas
 * @version 1.1.7
 * @date 2010-08-25
 */
public class MessageHandler extends Thread{	
	/**
	 * Mailbox of the erlang node
	 * @access private
	 */
	private OtpMbox otpMbox;
	/**
	 * Listeners registered
	 * @access private
	 */
	private Map<String, ArrayList<IInboxListener>> listeners;
	/**
	 * Name of thread.Default: inbox
	 * @access private
	 */
	private String name;
	
	/**
	 * Debug and other information logger
	 */
	private static Logger logger = Logger.getLogger(MessageHandler.class.getName());
	
	/**
	 * Class Constructor.
	 * Initializes the necessary parameters for running this thread
	 * @param name Thread name
	 * @param otpMbox A mailbox of an erlang node
	 * @param listeners Listeners
	 */
	public MessageHandler ( String name,
							OtpMbox otpMbox, 
						    Map<String, ArrayList<IInboxListener>> listeners ) {
		this.otpMbox = otpMbox;
		this.listeners = listeners;
		this.name = name;
		setName(this.name);
	}
	
	/**
	 * This thread runs until the system is shut down.
	 * Waits for incoming messages, analyzes and converts them before
	 * sending to listeners
	 */
	@Override
	public void run() {
		while (true) {
			try {
				/*
				 * Waiting for incoming messages.
				 * If the message cannot be converted or it is null,
				 * continues and waits for another message
				 */
				OtpErlangObject message = otpMbox.receive();
				logger.info("Message Received: " + message);
				if (message == null) {
					continue;
				}
				
				/*
				 * If the message is not an Erlang Tuple, we've received
				 * something wrong
				 */
				if (!(message instanceof OtpErlangTuple)) {
					continue;
				}
				
				/*
				 * If a tuple's arity(number of elements in tuple)
				 * is 0, something was wrong and 
				 * the processing will be skipped
				 */
				OtpErlangTuple tuple = (OtpErlangTuple) message;
				if (tuple.arity() == 0 ) {
					continue;
				}
				
				/*
				 * The first element of the tuple tells the concrete receiver and 
				 * handler of this message
				 */
				String key = tuple.elementAt(0).toString();
	            
				//In case that it has received {rex,ok}, skips processing
				if( tuple.arity() == 2 && 
					key.equals("rex") &&
					tuple.elementAt(1).toString().equals("ok"))
				{
					logger.info("Voucher Received! Skipping");
					continue;
				}
				
				/*
				 * A converter proxy will be searched for converting
				 * the message into a more readable format
				 */
	            ConverterExtensionPointProxy proxy = 
	            	ExtensionPointParser.getInstance().
	            	getErlangObjectConverterProxy(key);
				
				// when converter not found the OtpErlangObject is used
				Object converted = null;
				if (proxy == null) {
					converted = tuple;
				} else {
					converted = proxy.getConverter().convert(tuple);
				}
				
				// notify all "general" listeners
				if (listeners.containsKey("")) {
					for (IInboxListener listener : listeners.get("")) {
						listener.messageReceived(converted);
					}
				}
				
				// notify specific listeners
				if (listeners.containsKey(key)) {
					for (IInboxListener listener : listeners.get(key)) {
						listener.messageReceived(converted);
					}
				}
			} catch (OtpErlangExit e) {
				throw new ErlangException("Error when message received by the inbox", e);
			} catch (OtpErlangDecodeException e) {
				throw new ErlangException("Error when message received by the inbox", e);
			}
		}
	}
}
