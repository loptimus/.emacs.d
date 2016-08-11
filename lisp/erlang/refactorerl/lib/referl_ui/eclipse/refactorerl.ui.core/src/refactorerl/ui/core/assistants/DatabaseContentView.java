package refactorerl.ui.core.assistants;

import java.util.Collection;
import java.util.Map;
import java.util.StringTokenizer;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.ViewPart;

import refactorerl.ui.Activator;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.connection.ReferlUI;
import refactorerl.ui.core.constants.ReferlConstants;
import refactorerl.ui.core.inboxlisteners.RequestListener;
import refactorerl.ui.core.types.MessageType;
import refactorerl.ui.core.types.QueryResult;
import refactorerl.ui.core.types.QueryResult.QueryResultType;

public class DatabaseContentView extends ViewPart {
	private IConnection connection;
	private ScrolledForm form;
	private TreeViewer contentView;
	private String selectedFileName = ""; 
	
	@Override
	public void createPartControl(Composite parent) {
		FormToolkit toolkit = new FormToolkit(parent.getDisplay());
		form = toolkit.createScrolledForm(parent);
		form.setBounds(0, 0, 1000, 700);
		form.setText("RefactorErl, Database content");
		form.getBody().setLayout(new GridLayout(2, false));
		
		Button refreshButton = new Button(form.getBody(), SWT.BUTTON1);
		refreshButton.setText("Refresh");
		refreshButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) { }

			@Override
			public void widgetSelected(SelectionEvent e) {
				refreshContent();
			}
		});
		
		Button dropButton = new Button(form.getBody(), SWT.BUTTON1);
		dropButton.setText("Drop");
		dropButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) { }

			@Override
			public void widgetSelected(SelectionEvent e) {
				connection = ConnectionPool.getInstance().getConnection();
				
				if (null == connection) {
					MessageDialog.openWarning(
							Activator.getDefault().getWorkbench().getDisplay().getActiveShell(), 
							ReferlConstants.MESSAGE_NOCONFIG_HEADER,
							ReferlConstants.MESSAGE_NOCONFIG_BODY);
					return;
				}
				
				if (!selectedFileName.equals("")) {
    				if (!connection.ping()) {
    					connection.connect();
    				}
    				connection.getReferlUI().drop(selectedFileName).waitForResult();
    				refreshContent();
				}
			}
		});
		
		contentView = new TreeViewer(form.getBody());
		
		GridData gridData = new GridData();
		gridData.horizontalSpan = 2;
		gridData.horizontalAlignment = SWT.FILL;
		gridData.grabExcessHorizontalSpace = true;
		gridData.verticalAlignment = SWT.FILL;
		gridData.grabExcessVerticalSpace = true;
		contentView.getTree().setLayoutData(gridData);
		
		contentView.setContentProvider(new DatabaseContentProvider());
		contentView.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				if (event.getSelection() instanceof IStructuredSelection) {
					IStructuredSelection selection = (IStructuredSelection)event.getSelection();
					if (selection.getFirstElement() instanceof QueryResult) {
						QueryResult result = (QueryResult)selection.getFirstElement();
						
						if (QueryResultType.VALID_ELEMENT == result.getType()) {
							int startPos = result.getStartPos();
							int endPos = result.getEndPos();
							String file = result.getFile();
							
							if(!file.toUpperCase().equals(getFullFilename().toUpperCase())) {
								changeToFile(file);
							}
							selectArea(startPos, endPos);
							selectedFileName = file;
						}
					}
				}
			}
		});
	}
	
	private String getFullFilename() {
		IEditorPart currentEditor = PlatformUI.getWorkbench().
				getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		if (currentEditor != null) {
			IEditorInput editorInput = currentEditor.getEditorInput();
			if(editorInput instanceof IFileEditorInput) {
				IFileEditorInput fileInput = (IFileEditorInput) editorInput;
				IFile file = fileInput.getFile();
				return file.getLocation().toString();
			}
		}
		
		return "";
	}
	
	private void selectArea(int startPos, int endPos) {
		IEditorPart currentEditor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		if(currentEditor != null) {
			ISelectionProvider selectionProvider = currentEditor.getEditorSite().getSelectionProvider();	
			
			selectionProvider.addSelectionChangedListener(new ISelectionChangedListener() {
				public void selectionChanged( SelectionChangedEvent event) {
				}
			});
			selectionProvider.setSelection(new TextSelection(startPos -1, endPos - startPos));
		}
	}
	
	private void changeToFile(String file) {
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		Path path = new Path(file);
		IFile toOpen = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
		IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(toOpen.getName());
		IEditorReference reference = null;
		
		for (IEditorReference ref : page.getEditorReferences()) {
			try {
				StringTokenizer st = new StringTokenizer(path.toPortableString(),"/");
				String token = null;
				
				while (st.hasMoreElements()) {
					token = st.nextToken();
				}
				
				if (ref.getEditorInput().getName().equals(token)){
					reference = ref;
					break;
				}
			} catch (PartInitException e) {
				e.printStackTrace();
			}
		}
		
		try {
			if(reference != null) {
				IEditorInput input = reference.getEditorInput();
				page.openEditor(input, desc.getId());
			} else {
				IFileStore fileStore = EFS.getLocalFileSystem().getStore(path);
				IDE.openEditorOnFileStore(page, fileStore);
			}
		} catch (PartInitException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void setFocus() {
		form.setFocus();
		refreshContent();
	}
	
	private void refreshContent() {
		connection = ConnectionPool.getInstance().getConnection();
		if (null == connection) {
			MessageDialog.openWarning(
					Activator.getDefault().getWorkbench().getDisplay().getActiveShell(), 
					ReferlConstants.MESSAGE_NOCONFIG_HEADER,
					ReferlConstants.MESSAGE_NOCONFIG_BODY);
			return;
		}
		
		if (!connection.ping()) {
			connection.connect();
		}
		
		ReferlUI referlUI = connection.getReferlUI();
		RequestListener request = referlUI.executeSemanticQuery("mods", "", 0);
		
		Map<MessageType, Object> result = request.getResultMap();
		
		QueryResult root = new QueryResult("root");
		for (Object groupBy : (Collection<?>)result.get(MessageType.result)) {
			root.add((QueryResult)groupBy);
		}
		
		contentView.setInput(root);
		form.update();
		selectedFileName = "";
	}
	
	private class DatabaseContentProvider implements ITreeContentProvider {
		@Override
		public Object[] getChildren(Object element) {
			return ((QueryResult)element).getElements().toArray();
		}

		@Override
		public Object getParent(Object element) {
			return null;
		}

		@Override
		public boolean hasChildren(Object element) {
			return ((QueryResult)element).getElements().size() > 0;
		}

		@Override
		public Object[] getElements(Object inputElement) {
			return ((QueryResult)inputElement).getElements().toArray();
		}

		@Override
		public void dispose() {
			// Nothing to dispose
		}

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// TODO Auto-generated method stub
		}
	}
}
