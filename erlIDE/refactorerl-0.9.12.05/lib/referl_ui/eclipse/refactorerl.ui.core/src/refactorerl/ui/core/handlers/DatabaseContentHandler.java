package refactorerl.ui.core.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.views.IViewDescriptor;
import org.eclipse.ui.views.IViewRegistry;

public class DatabaseContentHandler extends AbstractHandler implements IHandler {
	private static final String DB_CONTENT_VIEW = "refactorerl.ui.core.views.databasecontent";
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IViewRegistry views = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getWorkbench().getViewRegistry();
		IViewDescriptor[] allViews = views.getViews();
		
		for (IViewDescriptor view : allViews) {
			if (view.getId().equals(DB_CONTENT_VIEW)) {
				try {
					IWorkbenchPage[] pages = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPages();
					for (IWorkbenchPage page : pages) {
						page.showView(DB_CONTENT_VIEW);
					}
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
		}
		
		return null;
	}
}
