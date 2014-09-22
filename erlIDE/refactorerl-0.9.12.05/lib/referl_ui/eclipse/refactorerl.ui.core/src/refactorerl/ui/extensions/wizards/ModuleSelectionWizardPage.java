package refactorerl.ui.extensions.wizards;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.MessageBox;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangList;

import refactorerl.ui.Images;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.inboxlisteners.RequestListener;
import refactorerl.ui.core.parameterproviders.ManualTransformParameterProvider;

/**
 * This is a wizard page to select a module from the current selected project. It is used for move Refactors. The same refactors can be done
 * via DragAndDrop, but sometimes it is more comfortable to select the target module in a wizard page
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */
public class ModuleSelectionWizardPage extends ManualTransformParameterProvider {
	private ListViewer viewer;

	public ModuleSelectionWizardPage(ExecutionEvent event, String name, String description, ArrayList<String> args) {
		super(event, name, description, args);
	}

	@Override
	public void createControl(Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		setControl(composite);

		composite.setLayout(new GridLayout(1, false));
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		viewer = new ListViewer(composite, SWT.BORDER);
		viewer.setContentProvider(new ArrayContentProvider());
		viewer.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {

				if (element instanceof IFile) {
					IFile file = (IFile) element;
					return file.getName().replace("." + file.getFileExtension(), "");
				}
				if (element instanceof String) return (String)element;
				return super.getText(element);
			}

			@Override
			public Image getImage(Object element) {
				if (element instanceof IFile) {
					return Images.ERLANG_IMG;
				}
				
				return super.getImage(element);
			}
		});
		
		viewer.getList().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				validate();
				if (viewer.getSelection() == null || viewer.getSelection().isEmpty()) return;
				String s=viewer.getSelection().toString();
				String modulename= s.substring(0, s.length()-5);
				modulename = modulename.substring(modulename.lastIndexOf('/')+1);
				if (s.endsWith(".erl]")) addParameter(NAME,modulename);
			}
		});

		try {
			IConnection connection = ConnectionPool.getInstance().getConnection();
			if (!connection.ping()) {
				connection.connect();
			}
			
			final List<String> files = new ArrayList<String>();
			RequestListener req = connection.getReferlUI().fileList();
			OtpErlangObject message = req.getResult();
			
			if (message instanceof OtpErlangTuple && ((OtpErlangTuple)message).arity()>1 && ((OtpErlangTuple)message).elementAt(1) instanceof OtpErlangList ) {
				OtpErlangList filelist = (OtpErlangList) ((OtpErlangTuple)message).elementAt(1);
				for (OtpErlangObject o : filelist) {
					if (!(o instanceof OtpErlangTuple && ((OtpErlangTuple)o).arity()>1)) continue;
					
					String filename= ((OtpErlangTuple)o).elementAt(0).toString();
					if (filename.endsWith(".erl\"")) {
						files.add(filename.substring(1,filename.length()-1));
					}
				}
			}
			viewer.setInput(files);
			validate();
		} catch (ErlangException e) {
	        MessageBox messageBox = new MessageBox(null, SWT.ICON_WARNING | SWT.OK);
	        messageBox.setText("RefactorErl");
	        messageBox.setMessage(e.getMessage());
		}
	}

	private void validate() {
		if (viewer.getSelection() == null || viewer.getSelection().isEmpty()) {
			setErrorMessage("Please select a module");
		} else {
			setErrorMessage(null);
		}
		setPageComplete(getErrorMessage() == null);
	}
}
