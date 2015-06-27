package refactorerl.ui.core.inboxlisteners;

import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.widgets.Display;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import refactorerl.ui.core.assistants.QuestionDialog;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.connection.IInboxListener;
import refactorerl.ui.core.types.MessageType;

/**
 * RequestLisyener class has the responsibility of handling a unique message.
 * Every message has a unique Id which is needed for the objects of this class
 * to identify the message to be handled. Every time when the user communicates
 * with RefactorErl, the system first asks for a uniqueId. With the new Id an object
 * of this class will be created and used for completing the request
 * 
 * @author Gergely Horvath, modified by J�zsa B�la - Tam�s
 * 
 */
public class RequestListener implements IInboxListener, Runnable {
	/**
	 * The unique identifier used in the communication
	 */
	private OtpErlangObject req = null;
	/**
	 * Connection with RefactorErl
	 */
	private IConnection conn = null;
	/**
	 * That green progress bar when you are working
	 */
	private IProgressMonitor progressMonitor;
	/**
	 * A question queue for multiple questions
	 */
	private ConcurrentLinkedQueue<OtpErlangTuple> questionQueue = 
		new ConcurrentLinkedQueue<OtpErlangTuple>();
	/**
	 * Answers received
	 */
	private OtpErlangList answers = null;
	/**
	 * It's complicated but needed. See the doc
	 */
	private CountDownLatch countDownLatch = new CountDownLatch(1);
	/**
	 * Object that results from the conversion of raw message
	 */
	private OtpErlangObject result = null;
	private Map<MessageType, Object> resultMap = null;
	
	/**
	 * Class constructor.
	 * @param cdl
	 * @param reqId
	 * @param connection
	 */
	public RequestListener( OtpErlangObject reqId, 
							IConnection connection ) {
		req = reqId;
		conn = connection;
	}

	/**
	 * Class constructor
	 * @param pm
	 * @param cdl
	 * @param reqId
	 * @param connection
	 */
	public RequestListener( IProgressMonitor pm, 
							OtpErlangObject reqId, 
							IConnection connection ) {
		req = reqId;
		conn = connection;
		progressMonitor = pm;
	}

	/**
	 * Class constructor
	 * @param cdl
	 * @param reqId
	 */
	public RequestListener(OtpErlangObject reqId) {
		req = reqId;
		conn = ConnectionPool.getInstance().getConnection();
	}
	
	/**
	 * Class constructor
	 * @param pm
	 * @param cdl
	 * @param reqId
	 */
	public RequestListener(IProgressMonitor pm, OtpErlangObject reqId) {
		req = reqId;
		conn = ConnectionPool.getInstance().getConnection();
		progressMonitor = pm;
	}
	
	/**
	 * Message handling process is executed here
	 * @param message Object the most recent 
	 * message received and destined to this object
	 */
	@Override
	public void messageReceived(Object message) {
		if (message instanceof OtpErlangTuple)  {
			OtpErlangTuple tuple = (OtpErlangTuple)message;
			
			if (tuple.arity() <= 2) { 
				return;
			}
			
			if (tuple.elementAt(1).toString().equals("reply") && tuple.elementAt(2) instanceof OtpErlangTuple) {
				ReplyHandler rh = new ReplyHandler(this);
				result = rh.handle(tuple);
				resultMap = rh.parseMessage(tuple);
				
				if (result != null) {
					removeListener();
				}
			} else if (tuple.elementAt(1).toString().equals("progress") && tuple.elementAt(2) instanceof OtpErlangTuple) {
				tuple = (OtpErlangTuple)tuple.elementAt(2);
				
				if (tuple.arity() == 6 && tuple.elementAt(3) instanceof OtpErlangLong && tuple.elementAt(4) instanceof OtpErlangLong) {
					try {
						int curr = ((OtpErlangLong)tuple.elementAt(3)).intValue();
						int max  = ((OtpErlangLong)tuple.elementAt(4)).intValue();
						
    					if (null == progressMonitor)  {
    						progressMonitor = new NullProgressMonitor();
    					}
    					
    					if (curr == 1) {
    						progressMonitor.beginTask(
    								((OtpErlangString)tuple.elementAt(1)).stringValue(),
    								max);
    					}
    					
						progressMonitor.worked(1);
    					
    					if (curr == max) {
    						progressMonitor.done();
    					}
					} catch (OtpErlangRangeException e) {
						e.printStackTrace();
					}
				}
			} else if ((tuple.elementAt(1).toString().equals("question") && tuple.elementAt(2) instanceof OtpErlangTuple)) {
				tuple = (OtpErlangTuple)tuple.elementAt(2);
				
				if ((tuple.arity() < 2) || !(tuple.elementAt(1) instanceof OtpErlangList)) {
					return;
				}
				
				questionQueue.add(tuple);
				OtpErlangObject qid = tuple.elementAt(0);
				Display.getDefault().syncExec(this);
				CountDownLatch cdl = new CountDownLatch(1);
				
				if (answers != null) {
					conn.getReferlUI().reply(qid, answers);
					answers = null;
				} else {
					conn.getReferlUI().cancel(qid);
				}
				
				try {
					cdl.await();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	@Override
	public void run() {
		if (questionQueue.isEmpty()) {
			return;
		}
		
		OtpErlangTuple tuple = questionQueue.remove();
		OtpErlangList questionList = (OtpErlangList)tuple.elementAt(1);
		QuestionDialog questionDialog = new QuestionDialog(Display.getCurrent().getActiveShell());
		answers = questionDialog.open(questionList);
	}
	
	public void waitForResult() {
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
	
	public OtpErlangObject getResult() {
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	public Map<MessageType, Object> getResultMap() {
		try {
			countDownLatch.await();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return resultMap;
	}
	
	/**
	 * @param progressMonitor the progressMonitor to set
	 */
	public void setProgressMonitor(IProgressMonitor progressMonitor) {
		this.progressMonitor = progressMonitor;
	}

	/**
	 * @return the progressMonitor
	 */
	public IProgressMonitor getProgressMonitor() {
		return progressMonitor;
	}
	
	public void removeListener() {
		conn.removeListener(req.toString(), this);
		
		if (countDownLatch != null) {
			countDownLatch.countDown();
		}
	}
}
