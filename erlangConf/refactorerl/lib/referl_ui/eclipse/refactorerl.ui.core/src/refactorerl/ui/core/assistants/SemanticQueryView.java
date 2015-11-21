package refactorerl.ui.core.assistants;

import java.util.Collection;
import java.util.Map;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
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

public class SemanticQueryView extends ViewPart {
	private IConnection connection;
	private ScrolledForm form;
	private TreeViewer queryView;
	
	/**
	  * This is a callback that will allow us to create the viewer and
	  * initialize it.
	  */
	@Override
	public void createPartControl(Composite parent) {
		FormToolkit toolkit = new FormToolkit(parent.getDisplay());
		form = toolkit.createScrolledForm(parent);
		form.setBounds(0, 0, 1000, 700);
		form.setText("RefactorErl, Semantic Queries");
		form.getBody().setLayout(new GridLayout(3, false));
		
		Label label = new Label(form.getBody(), SWT.NULL);
		label.setText("Query:");
		label.setBackground(form.getBackground());
		
		final Text queryString = new Text(form.getBody(), SWT.BORDER);
		queryString.setBackground(form.getBackground());
		queryString.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		Button button = new Button(form.getBody(), SWT.BUTTON1);
		button.setText("Execute");
		button.addSelectionListener(new SelectionListener(){
			public void widgetSelected(SelectionEvent e) {
				executeQuery(queryString.getText());
			}
			public void widgetDefaultSelected(SelectionEvent e) { }
		});
		
		queryView = new TreeViewer(form.getBody());
		
		GridData gridData = new GridData();
		gridData.horizontalSpan = 3;
		gridData.horizontalAlignment = SWT.FILL;
		gridData.grabExcessHorizontalSpace = true;
		gridData.verticalAlignment = SWT.FILL;
		gridData.grabExcessVerticalSpace = true;
		queryView.getTree().setLayoutData(gridData);
		
		queryView.setContentProvider(new QueryResultContentProvider());
		queryView.addSelectionChangedListener(new ISelectionChangedListener() {
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
						}
					}
				}
			}
		});
	}
	
	@Override
	public void dispose() {
		super.dispose();
	}

	private String getFullFilename(){
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

	private int getCursorPosition() {
		IEditorPart currentEditor = PlatformUI.getWorkbench().
			getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		if(currentEditor != null) {
			ISelection selection = 
				currentEditor.getEditorSite().getSelectionProvider().getSelection();
			if(selection instanceof TextSelection) {
				TextSelection textSelection = (TextSelection) selection;
				return textSelection.getOffset();
			}
		}
		return 0;
	}
	
	/**
	  * Passing the focus request to the form.
	  */
	@Override
	public void setFocus() {
		form.setFocus();
	}
	
	private void executeQuery(String query) {
		String file = getFullFilename();
		int position = getCursorPosition();
		
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
		RequestListener request = referlUI.executeSemanticQuery(query, file, position);
		
		Map<MessageType, Object> result = request.getResultMap();
		displayResult((Collection<?>)result.get(MessageType.result));
	}
	
	private void displayResult(Collection<?> result) {
		QueryResult root = new QueryResult("root");
		
		for (Object groupBy : result) {
			root.add((QueryResult)groupBy);
		}
		queryView.setInput(root);
		
		form.update();
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
	
	private class QueryResultContentProvider implements ITreeContentProvider {
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
