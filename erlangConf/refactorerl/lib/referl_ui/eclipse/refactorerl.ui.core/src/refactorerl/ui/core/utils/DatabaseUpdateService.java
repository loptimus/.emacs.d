package refactorerl.ui.core.utils;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.erlide.core.CoreScope;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlModelChangeListener;

import refactorerl.ui.Activator;
import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.connection.ReferlUI;
import refactorerl.ui.core.constants.ReferlConstants;

/**
 * Single instance to keep the model uptodate in the database. When the core plugin is started this service is also started. At instantiation all
 * of the Erlang projects' modules are synchronized. Between 2 refactorings the service tracks the changes and before any refactor it loads the
 * changes into the Database.
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * @author B�la - Tam�s J�zsa, logging added. Db restart error resolved
 */
public class DatabaseUpdateService implements IErlModelChangeListener {
	private static DatabaseUpdateService instance;

	private Set<String> changes = Collections.synchronizedSet(new HashSet<String>());

	private boolean dbSync;
	private boolean dbSyncReset;
	private static Logger logger = Logger.getLogger(DatabaseUpdateService.class.getName());

	public DatabaseUpdateService() {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		dbSync = store.getBoolean(ReferlConstants.KEY_REFACTORERL_DBSYNC);
		dbSyncReset = store.getBoolean(ReferlConstants.KEY_REFACTORERL_DBSYNC_RESET);
	}

	/**
	 * It is called from the plugin Activator class at starttime
	 * 
	 * @return single instance of this class
	 */
	public static DatabaseUpdateService getInstance() {
		if (null == instance) {
			instance = new DatabaseUpdateService();

			if (instance.dbSync) {
				CoreScope.getModel().addModelChangeListener(instance);
			}
		}
		return instance;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.core.erlang.IErlModelChangeListener#elementChanged(org.erlide.core.erlang.IErlElement)
	 */
	@Override
	public void elementChanged(IErlElement changedElement) {
		String path = WorkspaceUtils.pathToString(changedElement.getResource().getFullPath());
		changes.add(path);
	}

	/**
	 * Synchronizes to the database and releases the current changes to let the service start gathering only the latest changes
	 * 
	 * @param referlUI
	 * @throws ErlangException 
	 */
	public void synchronizeChanges(ReferlUI referlUI) throws ErlangException {
		if (referlUI == null) {
			throw new IllegalArgumentException("ReferlUI parameter is required");
		}

		for (String path : changes) {
			referlUI.add(path);
		}

		changes = new HashSet<String>();
	}

	/**
	 * It does a full synchronization of the workspace to the database
	 * 
	 * @param referlUI
	 * @throws ErlangException 
	 */
	public void fullSynchronize(ReferlUI referlUI) throws ErlangException {
		if (referlUI == null) {
			throw new IllegalArgumentException("ReferlUI parameter is required");
		}

		// reset the database if it is configured in the preferences
		if (dbSyncReset) {
			logger.info("Reseting Database");
			referlUI.resetDatabase();
			try {
				logger.info("Waiting for RefactorErl");
				Thread.sleep(5000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			logger.info("Reset Finished");
		}

		final Set<String> paths = new HashSet<String>();
		try {
			for (IProject project : WorkspaceUtils.getErlangProjects()) {
				project.accept(new IResourceProxyVisitor() {
					@Override
					public boolean visit(IResourceProxy proxy) throws CoreException {
						if (proxy.getName().endsWith(".erl") || proxy.getName().endsWith(".hrl")) {
							paths.add(WorkspaceUtils.pathToString(proxy.requestFullPath()));
						}

						return true;
					}
				}, 0);
			}
		} catch (CoreException e) {
			throw new ErlangException("Could not sync to database", e);
		}

		for (String path : paths) {
			referlUI.add(path);
		}

		changes = Collections.synchronizedSet(new HashSet<String>());
	}

	/**
	 * clears the single instance and forces new instance creation when 
	 * getInstance gets called next time
	 */
	public void reset() {
		CoreScope.getModel().removeModelChangeListener(this);
		instance = null;
	}
}
