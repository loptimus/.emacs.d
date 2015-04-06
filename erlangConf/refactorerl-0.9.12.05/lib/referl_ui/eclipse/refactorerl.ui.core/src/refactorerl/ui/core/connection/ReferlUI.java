package refactorerl.ui.core.connection;

import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;

import refactorerl.ui.core.constants.ReferlConstants;
import refactorerl.ui.core.constants.ReferlArgsConstants;
import refactorerl.ui.core.inboxlisteners.RequestListener;
import refactorerl.ui.core.utils.DatabaseUpdateService;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Represents the Erlang refactor subsystem written in Erlang language in java. The services of the refactoring tool is accessible through this
 * facade from Java.
 * 
 * @author zoltan verebes, modified by Horv�th Gergely
 */
public class ReferlUI implements ReferlConstants {
	private IConnection connection;

	/**
	 * All of the connections have exactly one referlUI. ReferlUI knows about its connection by passing it to this constructor
	 * 
	 * @param connection
	 */
	public ReferlUI(IConnection connection) {
		this.connection = connection;
	}

	/**
	 * Class referl_ui:add(path) function. It is to add a new element to the database or update an old one. When the file is already part of the
	 * database a drop function call is also indicated by Refactorerl.
	 * 
	 * @param path
	 *            of the file to be added to the database
	 */
	public RequestListener add(String path) {
		return callFunWithPath(null, FUN_ADD, path);
	}
	
	public RequestListener addDirectory(IProgressMonitor pm, String path) {
		return callFunWithPath(pm, FUN_ADD_DIR, path);
	}

	/**
	 * Class referl_ui:drop(path) function. It is to drop an element from the database. When the file is not part of the database an error
	 * message is sent by the Erlang process
	 * 
	 * @param path
	 *            of the file to be dropped to the database
	 */
	public RequestListener drop(String path) {
		return callFunWithPath(null, FUN_DROP, path);
	}

	/**
	 * Common, convenience function to call a fun on the referl_ui module.
	 * 
	 * @param funName
	 * @param path
	 */
	public RequestListener callFunWithPath(IProgressMonitor pm, String funName, String path) {
		if (null == path || "".equals(path)) {
			throw new IllegalArgumentException("Path cannot be null or empty");
		}

		if (null == funName || "".equals(funName)) {
			throw new IllegalArgumentException("funName cannot be null or empty");
		}
		
		return sendRequest(pm, funName, new OtpErlangString(path));
	}

	/**
	 * calls the transform function on referl_ui module. args map is converted into OtpErlangObject as defined the the ReferlArgsUtil class
	 * 
	 * @param name
	 * @param args
	 */
	public RequestListener transform(String name, Map<String, Object> args) {
		return transform(null, name, args);
	}
	
	public RequestListener transform(IProgressMonitor pm, String name, Map<String, Object> args) {
		if (null == name || name.equals("")) {
			throw new IllegalArgumentException("Name parameter cannot be null!");
		}
		
		DatabaseUpdateService.getInstance().synchronizeChanges(this);

		if (null == args) {
			return sendRequest(pm, FUN_TRANSFORM, new OtpErlangObject[] { new OtpErlangString(name) });
		} else {
			args.put(ReferlArgsConstants.MISSING, "true");
			OtpErlangList arglist = ReferlArgsUtil.mapToArglist(args);
			return sendRequest(pm, FUN_TRANSFORM,  new OtpErlangObject[] { new OtpErlangAtom(name), arglist });
		}
	}

	public RequestListener reply(OtpErlangObject questionId, OtpErlangList answers) {
		OtpErlangObject arg[] = new OtpErlangObject[] { questionId, answers};
		return sendRequest(FUN_REPLY, arg);
	}
	
	public RequestListener cancel(OtpErlangObject questionId) {
		OtpErlangObject arg[] = new OtpErlangObject[]{ questionId };
		return sendRequest(FUN_CANCEL, arg);
	}
	
	public RequestListener clustering(String entityType, String algorithm) {
		return sendRequest(
				"transform",
				new OtpErlangAtom("clustering"),
				new OtpErlangList(
						new OtpErlangObject[] {
								new OtpErlangTuple(
										new OtpErlangObject[] {
												new OtpErlangAtom("ask_missing"),
												new OtpErlangAtom("true")
										}),
								new OtpErlangTuple(
										new OtpErlangObject[] {
												new OtpErlangAtom("algorithm"),
												new OtpErlangAtom(algorithm)
										}),
								new OtpErlangTuple(
										new OtpErlangObject[] {
												new OtpErlangAtom("entity"),
												new OtpErlangAtom(entityType)
										})
						})
				);
	}
	
	public RequestListener executeSemanticQuery(String query, String file, int position) {
		OtpErlangTuple displayOpt =
			 new OtpErlangTuple(
				 new OtpErlangObject[] {
						 new OtpErlangAtom("display_opt"),
						 new OtpErlangList(
								 new OtpErlangObject[] {
										 new OtpErlangTuple(
												 new OtpErlangObject[] {
														 new OtpErlangAtom("positions"),
														 new OtpErlangAtom("scalar")
												 }),
										 new OtpErlangTuple(
												 new OtpErlangObject[] {
														 new OtpErlangAtom("output"),
														 new OtpErlangAtom("other")
												 }),
								 }
						  )
				  });
		
		OtpErlangTuple startOpt =
			new OtpErlangTuple(
					new OtpErlangObject[] {
							new OtpErlangAtom("start_opt"),
							new OtpErlangList(
									new OtpErlangObject[] {
											new OtpErlangTuple(
													new OtpErlangObject[] {
															new OtpErlangAtom("ask_missing"),
															new OtpErlangAtom("false")
													}),
											new OtpErlangTuple(
													new OtpErlangObject[] {
															new OtpErlangAtom("position"),
															new OtpErlangLong(position)
													}),
											new OtpErlangTuple(
													new OtpErlangObject[] {
															new OtpErlangAtom("file"),
															new OtpErlangString(file)
													})
									})
					});
		
		return
			sendRequest(
					"transform",
					new OtpErlangAtom("semantic_query"),
					new OtpErlangList(
							new OtpErlangObject[] {
									displayOpt,
									startOpt,
									new OtpErlangTuple(
											new OtpErlangObject[] {
													new OtpErlangAtom("querystr"), 
													new OtpErlangString(query)	  
											})
							}));
	}
	
	/**
	 * calls the filelist function on the referl_ui module. reply is processed by an inbox listener
	 */
	public RequestListener fileList() {
		return sendRequest(FUN_FILELIST);
	}

	/**
	 * calls reset function on referl_ui module to clear all of the files from the database
	 */
	public RequestListener resetDatabase() {
		return sendRequest(FUN_RESET_DB);
	}

	public RequestListener undo() {
		OtpErlangObject arg[]= new OtpErlangObject[] { new OtpErlangString("") };
		return sendRequest(FUN_UNDO, arg);
	}

	protected OtpErlangPid getPid() {
		OtpErlangPid pid = null;
		
		if (connection instanceof ReferlConnection) {
			pid = ((ReferlConnection)connection).getPid();
		}
		
		return pid;
	}
	
	protected OtpErlangTuple getId() {
		OtpErlangTuple ref = null; 
		
		if (connection instanceof ReferlConnection) {
			IdHandler handler =((ReferlConnection)connection).getIdHandler();
			ref = handler.requestId();
		}
 		
		return ref;
	}
	
	public RequestListener sendRequest(String reqName, OtpErlangObject... args) {
		return sendRequest(null, reqName, args);
	}
	
	public RequestListener sendRequest(IProgressMonitor pm, String reqName, OtpErlangObject... args) {
		OtpErlangTuple request = getId();
		OtpErlangPid pid = getPid();
		
		RequestListener reqHandler = new RequestListener(pm, request, connection);
		
		connection.addListener(request.toString(), reqHandler);
		
		OtpErlangObject[] arg = new OtpErlangObject[args.length + 1];
		
		arg[0] = new OtpErlangAtom(reqName);
		for (int i = 0; i < args.length; ++i) {
			arg[i + 1] = args[i];
		}
		
		connection.send(
				UI_ROUTER,
				FUN_REQUEST,
				new OtpErlangObject[] {
						new OtpErlangTuple(
								new OtpErlangObject[] {
										pid,
										new OtpErlangAtom(NODE)
								}),
						request,
						new OtpErlangTuple(arg)
				});
		
		return reqHandler;
	}
	
	public boolean isSystemUp() {
		return connection.ping();
	}
}
