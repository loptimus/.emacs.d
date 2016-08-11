package refactorerl.ui.core.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import refactorerl.ui.Activator;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.constants.ReferlConstants;
import refactorerl.ui.core.utils.ConnectionUtils;

public class RefactorerlPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage, ReferlConstants {
	private DirectoryFieldEditor referlDir;
	private DirectoryFieldEditor workingDir;
	private IntegerFieldEditor startWaitingTime;
	private BooleanFieldEditor startProcess;
	private BooleanFieldEditor autoDBSync;
	private BooleanFieldEditor autoDBSyncReset;
	private ComboFieldEditor dbType;
	private String[][] dbTypeValues = {{"Mnesia", "mnesia"},{"Nif graph", "nif"}};
	
	public RefactorerlPreferencePage() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
	 */
	@Override
	protected void createFieldEditors() {
		startProcess = new BooleanFieldEditor(KEY_REFACTORERL_START_PROCESS, "Start background process automaticaly", getFieldEditorParent()) {

			@Override
			protected void valueChanged(boolean oldValue, boolean newValue) {
				super.valueChanged(oldValue, newValue);
				referlDir.setEnabled(newValue, getFieldEditorParent());
				workingDir.setEnabled(newValue, getFieldEditorParent());
				startWaitingTime.setEnabled(newValue, getFieldEditorParent());
				dbType.setEnabled(newValue, getFieldEditorParent());
			}

		};

		referlDir  = new DirectoryFieldEditor(KEY_REFACTORERL_REFERL_DIR, "RefactorErl directory: ", getFieldEditorParent());
		workingDir = new DirectoryFieldEditor(KEY_REFACTORERL_WORKING_DIR, "Working directory: ", getFieldEditorParent());
		startWaitingTime = new IntegerFieldEditor(KEY_REFACTORERL_START_WAITINGTIME, "Waiting time when process started in seconds: ",
				getFieldEditorParent());

		autoDBSync = new BooleanFieldEditor(KEY_REFACTORERL_DBSYNC, "Synchronize workspace automaticaly when model changed",
				getFieldEditorParent()) {
			@Override
			protected void valueChanged(boolean oldValue, boolean newValue) {
				super.valueChanged(oldValue, newValue);
				autoDBSyncReset.setEnabled(newValue, getFieldEditorParent());
			}
		};

		autoDBSyncReset = new BooleanFieldEditor(KEY_REFACTORERL_DBSYNC_RESET, "Reset database before full synchronization",
				getFieldEditorParent());
		
		dbType = new ComboFieldEditor(KEY_REFACTORERL_DB_TYPE, "Database type: ", dbTypeValues, getFieldEditorParent());

		addField(startProcess);
		addField(referlDir);
		addField(workingDir);
		addField(startWaitingTime);
		addField(autoDBSync);
		addField(autoDBSyncReset);
		addField(dbType);

		referlDir.getTextControl(getFieldEditorParent()).addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				validate();
			}
		});

		workingDir.getTextControl(getFieldEditorParent()).addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				validate();
			}
		});

		startWaitingTime.getTextControl(getFieldEditorParent()).addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				validate();
			}
		});

		referlDir.setEnabled(getPreferenceStore().getBoolean(KEY_REFACTORERL_START_PROCESS), getFieldEditorParent());
		workingDir.setEnabled(getPreferenceStore().getBoolean(KEY_REFACTORERL_START_PROCESS), getFieldEditorParent());
		startWaitingTime.setEnabled(getPreferenceStore().getBoolean(KEY_REFACTORERL_START_PROCESS), getFieldEditorParent());
		autoDBSyncReset.setEnabled(getPreferenceStore().getBoolean(KEY_REFACTORERL_DBSYNC), getFieldEditorParent());
		dbType.setEnabled(getPreferenceStore().getBoolean(KEY_REFACTORERL_START_PROCESS), getFieldEditorParent());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createContents(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		Control result = super.createContents(parent);
		validate();
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(IWorkbench workbench) {
	}

	private void validate() {
		if (startProcess.getBooleanValue() && referlDir.getTextControl(getFieldEditorParent()).getText().equals("")) {
			setErrorMessage("RefactorErl directory is required");
			return;
		}

		if (startProcess.getBooleanValue() && workingDir.getTextControl(getFieldEditorParent()).getText().equals("")) {
			setErrorMessage("Working directory is required");
			return;
		}

		if (startProcess.getBooleanValue() && startWaitingTime.getTextControl(getFieldEditorParent()).getText().equals("")) {
			setErrorMessage("Start waiting time is required");
			return;
		}

		setErrorMessage(null);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.PreferencePage#performApply()
	 */
	@Override
	protected void performApply() {
		super.performApply();

		if (ConnectionUtils.validatePreferences(getPreferenceStore())) {
			ConnectionPool.getInstance().reset();
			
			IConnection connection = ConnectionPool.getInstance().getConnection();
			ConnectionUtils.configureConnection(connection, getPreferenceStore().getBoolean(KEY_REFACTORERL_DBSYNC));
		}
	}

}
