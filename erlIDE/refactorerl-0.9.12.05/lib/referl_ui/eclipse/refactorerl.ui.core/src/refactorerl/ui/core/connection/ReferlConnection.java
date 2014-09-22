package refactorerl.ui.core.connection;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.core.runtime.Path;

import refactorerl.ui.Activator;
import refactorerl.ui.core.constants.ReferlConstants;
import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.handlers.MessageHandler;
import refactorerl.ui.core.handlers.RefactorErlProcessHandler;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;


/**
 * Default implementation of IConnection class. For further info see IConnection
 * 31-08-2010 - Documentation Added. Nested Classes exported. Logging added.
 * @author zoltan verebes, modified by Horv�th Gergely, J�zsa B�la - Tam�s
 * 
 */
public class ReferlConnection implements IConnection, ReferlConstants, IInboxListener {
    protected IdHandler idHandler;
	protected Process refactorerlRuntime;
	
	protected OtpMbox inbox;
	protected OtpNode self;

	protected ReferlUI referlUI;
	protected Map<String, ArrayList<IInboxListener>> listeners;

	protected String cmd;
	protected String referlDir;
	protected String workingDir;
	protected boolean startProcess;
	protected int startWaitingTime;
	protected String dbType;
	protected boolean testEnvironment = false;
	private static Logger logger = Logger.getLogger(ReferlConnection.class.getName());
	
	private MessageHandler messageHandler;
	private boolean messageHandlerStarted;
	private boolean connected;
	private TemporaryInbox tempInbox;
	
	public ReferlConnection(Map<String, Object> preferences) {		
		referlDir  = (String)preferences.get(KEY_REFACTORERL_REFERL_DIR);
		workingDir = (String)preferences.get(KEY_REFACTORERL_WORKING_DIR);
		startProcess = (Boolean) preferences.get(KEY_REFACTORERL_START_PROCESS);
		startWaitingTime = (Integer) preferences.get(KEY_REFACTORERL_START_WAITINGTIME);
		dbType = (String) preferences.get(KEY_REFACTORERL_DB_TYPE);
		idHandler = new IdHandler(this);
		messageHandlerStarted = false;
		connected = false;
		
		cmd = new Path(referlDir).append("/bin/").toString() + "referl -server -base " + referlDir + " -db " + dbType;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.connection.IConnection#addListener(refactorerl.ui.core.connection.IInboxListener)
	 */
	@Override
	synchronized public void addListener(IInboxListener listener) {
		addListener("", listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.connection.IConnection#addListener(java.lang.String, refactorerl.ui.core.connection.IInboxListener)
	 */
	@Override
	synchronized public void addListener(String key, IInboxListener listener) {
		addProxiedListener(key, listener);
	}

	synchronized public void addProxiedListener(String key, IInboxListener listener) {    
		if (this.listeners == null) {
			return;
		}
	
		if (key == null) {
			key = "";
		}
	
		ArrayList<IInboxListener> keyListeners = this.listeners.get(key);
	
		if (keyListeners == null) {
			keyListeners = new ArrayList<IInboxListener>();
			listeners.put(key, keyListeners);
		}
		
		keyListeners.add(new ListenerProxy(listener));
	}

	public boolean canConnect() {
		return referlDir != null && !referlDir.equals("") && workingDir != null && !workingDir.equals("");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erlang.refactor.core.connection.IConnection#connect()
	 */
	@Override
	synchronized public boolean connect() {
		if (isConnected()) {
			return true;
		}
		
		//checking necessary parameters for connecting to RefactorErl
		if (!canConnect()) {
			MessageDialog.openWarning(
					Activator.getDefault().getWorkbench().getDisplay().getActiveShell(), 
					ReferlConstants.MESSAGE_NOCONFIG_HEADER,
					ReferlConstants.MESSAGE_NOCONFIG_BODY);
			return false;
		}
		
		listeners = Collections.synchronizedMap(
				new HashMap<String, ArrayList<IInboxListener>>());
		addListener(this); // TODO: remove
		addListener("rex", idHandler);
		
		if (startProcess) {
			startRefactorerlProcess();
		}
		
		logger.info("Initializing Erlang Node");
		initNode();
		logger.info("Node Initialized");
		
		while(!ping()) {			
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
		//Establishing Connection With RefactorErl
		int connectionAttempts = 0;
		while (!connected && connectionAttempts < 10) {
			connectionAttempts++;
			logger.info("Attempting to connect: " + connectionAttempts);
			
			send(UI_ROUTER, FUN_ADD_MSG_HANDLER, new OtpErlangObject[] { inbox.self() });
			try {
				OtpErlangObject answ = inbox.receive();
				logger.info("Message Received: " + answ);
				if (answ.toString().equals("installed")) {
					connected = true;
					logger.info("CONNECTED");
				} else {
					Thread.sleep(1000);
				}
			} catch (OtpErlangDecodeException e) {
				e.printStackTrace();
			} catch (OtpErlangExit e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		//Initialization finished
		
		/*
		 * Starting a messageHandler on a separate thread. It will have the role
		 * to receive incoming messages from RefactorErl
		 */
		if (!testEnvironment && connected) {
			/*
			 * when a new message is received all of the listeners 
			 * are notified with the received OtpErlangObject as a parameter
			 */
			messageHandler = new MessageHandler("inbox", inbox, listeners);
			/*
			 * The Handler is not started immediately. It will be started after
			 * the activator has finished configuring the system.
			 * See refactorerl.ui.core.Activator
			 */
			//startInbox();
			
			
			/* After a successful connection all commands available
			 * can be used from referlUI
			 * See Facade Design Pattern
			 */
			this.referlUI = new ReferlUI(this);
		}
		
		if (!connected) {
			resetListeners();
			terminateNode();
			killProcessIfRunning();
		}
		
		return connected;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.connection.IConnection#connectChecked()
	 */
	@Override
	synchronized public boolean connectChecked() {
		if (!connect()) {
			return false;
		}
		
		
		return ping();
	}

	public IdHandler getIdHandler() {
		return idHandler;
	}

	public OtpErlangPid getPid() throws ErlangException {
		if (inbox != null) {
			return inbox.self();
		} else {
			throw new ErlangException("Node isn't initialized");
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erlang.refactor.core.connection.IConnection#getReferlUI()
	 */
	@Override
	synchronized public ReferlUI getReferlUI() {
		return referlUI;
	}

	/**
	 * @return the connected
	 */
	public boolean isConnected() {
		return connected;
	}

	private void initNode() throws ErlangException {
		try {
			self = new OtpNode(REFACTORERL_ERLIDE);
			inbox = self.createMbox("inbox");
			tempInbox = new TemporaryInbox(inbox);
		} catch (IOException e) {
			throw new ErlangException("Initialization of the node failed", e);
		}
	}

	/**
	 * @return the messageHandlerStarted
	 */
	public boolean isMessageHandlerStarted() {
		return messageHandlerStarted;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.connection.IConnection#killProcessIfRunning()
	 */
	@Override
	public void killProcessIfRunning() {
		if (refactorerlRuntime == null) {
			return;
		}
	
		refactorerlRuntime.destroy();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.connection.IInboxListener#messageReceived(java.lang.Object)
	 */
	@Override
	public void messageReceived(Object message) {
		logger.info("RECEIVED:" + message);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erlang.refactor.core.connection.IConnection#ping()
	 */
	@Override
	synchronized public boolean ping() {
		if (inbox == null) {
			return false;
		}

		return inbox.ping(NODE, 1000);
	}

	synchronized public void removeListener(IInboxListener listener) {
		removeListener("", listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.connection.IConnection#removeListener(java.lang.String, refactorerl.ui.core.connection.IInboxListener)
	 */
	@Override
	synchronized public void removeListener(String key, IInboxListener listener) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erlang.refactor.core.connection.IConnection#send(java.lang.String, java.lang.String, com.ericsson.otp.erlang.OtpErlangObject[])
	 */
	@Override
	synchronized public void send(String module, String function, OtpErlangObject... args) {
		// {self, {call, Mod, Fun, Args, user}}
		OtpErlangTuple call = new OtpErlangTuple(
									new OtpErlangObject[] { 
										new OtpErlangAtom("call"), 
										new OtpErlangAtom(module),
										new OtpErlangAtom(function),
										new OtpErlangList(args), 
										new OtpErlangAtom("user") 
									}
								);
		logger.info("Sending: " + call);
		logger.info("Args: " + args);
		
		inbox.send("rex", NODE, new OtpErlangTuple(new OtpErlangObject[] { inbox.self(), call }));
		
		/* 
		 * If the system is in connection phase a temporary message handler 
		 * will be used until the main message handler thread will be started
		 */
		if(isConnected() && !isMessageHandlerStarted()) {
			temporaryMessageHandling();
		}
	}

	/**
	 * Starts the executing of the inbox Thread.
	 * It has the role of receiving messages form RefactorErl
	 * @return True if messageHandler was started or it is running and false
	 * if it was not initialized
	 */
	public boolean startInbox() {
		boolean result = true;
		
		if (messageHandler == null) {
			result = false;
		}
		
		if (!messageHandler.isAlive()) {
			addListener("rex", idHandler);
			messageHandler.start();
			messageHandlerStarted = true;
		}
		
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.connection.IConnection#startRefactorerlProcess()
	 */
	synchronized public void startRefactorerlProcess() throws ErlangException {
		if (cmd == null || cmd.equals("")) {
			throw new IllegalArgumentException("Process start command cannot be empty. Please check the preferences of Refactorerl");
		}
		
		final File workingDirectory = new File(workingDir);
		
		try {
			refactorerlRuntime = Runtime.getRuntime().exec(cmd, null, workingDirectory);
			
			final Thread thread = new RefactorErlProcessHandler(
					REFACTORERL_PROCESS_NAME, 
					refactorerlRuntime);
			thread.setDaemon(false);
			thread.start();
		} catch (IOException e) {
			e.printStackTrace();
			throw new ErlangException("Could not start Refactorerl process", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.connection.IConnection#resetListeners()
	 */
	@Override
	public void resetListeners() {
		if (listeners != null) {
			listeners.clear();
		}
	}

	private void temporaryMessageHandling() {
		OtpErlangTuple tup = tempInbox.receiveMessage();
		
		if (tup == null) {
			return;
		}
		
		if (tup.elementAt(0) instanceof OtpErlangAtom) {
			OtpErlangAtom to = (OtpErlangAtom)tup.elementAt(0);
			if(to.atomValue().equals("rex")) {
				idHandler.messageReceived(tup);
			}	
		}
	}
	
	private void terminateNode() {
		if (self != null) {
			self.closeMbox(inbox);
			self.close();
		}
	}
}
