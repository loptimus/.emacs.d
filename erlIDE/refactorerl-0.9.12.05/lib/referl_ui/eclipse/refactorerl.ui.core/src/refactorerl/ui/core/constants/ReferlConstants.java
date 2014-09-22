package refactorerl.ui.core.constants;

/**
 * Constants related to Erlang connection and Preferences keys in the preferences store
 * 
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */
public interface ReferlConstants {
	public static final String MESSAGE_NOCONFIG_HEADER = "RefactorErl start";
	public static final String MESSAGE_NOCONFIG_BODY = "Please, configure RefactorErl settings in the preferences dialog";
	
	public static final String NODE = "refactorerl@localhost";
	public static final String REFACTORERL_ERLIDE = "refactorerl_erlide";

	public static final String UI_ROUTER = "reflib_ui_router";
	public static final String FUN_ADD = "add";
	public static final String FUN_ADD_DIR = "add_dir";
	public static final String FUN_DROP = "drop";
	public static final String FUN_TRANSFORM = "transform";
	public static final String FUN_FILELIST = "filelist";
	public static final String FUN_RESET_DB = "reset";
	public static final String FUN_REPLY = "reply";
	public static final String FUN_CANCEL = "cancel";
	public static final String FUN_UNDO = "undo";
	public static final String FUN_START = "start";
	public static final String FUN_REQUEST = "request";
	public static final String FUN_ADD_MSG_HANDLER = "add_msg_handler";
	
	// TODO: let node name / cookie to be specified from outside
	public static final String REFACTORERL_PROCESS_NAME = "refactorerl";

	public static final String KEY_REFACTORERL_REFERL_DIR = "refactorerl.ui.preferences.referldir";
	public static final String KEY_REFACTORERL_PROCESS_NAME = "refactorerl.ui.preferences.processname";
	public static final String KEY_REFACTORERL_WORKING_DIR = "refactorerl.ui.preferences.workingdir";
	public static final String KEY_REFACTORERL_START_PROCESS = "refactorerl.ui.preferences.startprocess";
	public static final String KEY_REFACTORERL_START_WAITINGTIME = "refactorerl.ui.preferences.startwaitingtime";
	public static final String KEY_REFACTORERL_DBSYNC = "refactorerl.ui.preferences.dbsync";
	public static final String KEY_REFACTORERL_DBSYNC_RESET = "refactorerl.ui.preferences.dbsync.reset";
	public static final String KEY_REFACTORERL_DB_TYPE = "refactorerl.ui.preferences.dbtype";

	public static final String MOVE_REC = "referl_tr_move_rec";
	public static final String MOVE_FUN = "referl_tr_move_fun";
	public static final String MOVE_MAC = "referl_tr_move_mac";
	
	public static final String ERL_NATURE = "org.erlide.core.erlnature";
	
	public static final String CORE_PLUGIN = "refactorerl.ui.core";
	public static final String EXTENSIONS_PLUGIN = "refactorerl.ui.extensions";
}
