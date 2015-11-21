package refactorerl.ui.core.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.IElementComparer;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.editors.erl.ErlangEditor;
//import org.erlide.ui.util.ErlModelUtils;

import refactorerl.ui.Activator;
import org.erlide.core.model.util.ModelUtils;

@SuppressWarnings("restriction")
public final class SelectionUtils {
	/**
	 * Iterates through all of the tree paths in the selection and try to find the common prefix of the paths
	 * 
	 * @param selection
	 * @return common prefix path of all paths in the selection or null when common prefix not found
	 */
	public static TreePath getTreeSelectionTreePathPrefix(ITreeSelection selection) {
		if (selection.isEmpty()) {
			return null;
		}

		TreePath firstPath = selection.getPathsFor(selection.getFirstElement())[0];
		while (firstPath != null) {
			boolean prefix = true;
			for (TreePath path : selection.getPaths()) {
				if (!path.startsWith(firstPath, new IElementComparer() {

					@Override
					public boolean equals(Object a, Object b) {
						return a.equals(b);
					}

					@Override
					public int hashCode(Object element) {
						return element.hashCode();
					}

				})) {
					prefix = false;
					break;
				}
			}

			if (prefix) {
				return firstPath;
			}

			firstPath = firstPath.getParentPath();
		}

		return null;
	}

	public static String getVariable(ITextEditor editor, ITextSelection selection) throws BadLocationException {
		if (selection.getStartLine() != selection.getEndLine()) {
			return null;
		}

		IDocument document = editor.getDocumentProvider().getDocument(editor.getEditorInput());

		if (document == null) {
			return null;
		}

		IRegion region = document.getLineInformation(selection.getStartLine());
		String line = document.get(region.getOffset(), region.getLength());
		int selectionIndex = selection.getOffset() - region.getOffset();

		int start = line.replaceAll("[^A-Za-z0-9_@]", "?").substring(0, selection.getOffset() - region.getOffset()).lastIndexOf("?") + 1;
		int end = line.replaceAll("[^A-Za-z0-9_@]", "?").substring(selectionIndex).indexOf("?") + selectionIndex;

		String word = line.substring(start, end);
		return word.length() != 0 && (word.startsWith("_") || Character.isUpperCase(word.charAt(0))) ? word : null;
	}

	public static String convertStreamToString(IEditorInput input) throws CoreException {
		InputStream is = ((FileEditorInput) input).getFile().getContents();
		/*
		 * To convert the InputStream to String we use the BufferedReader.readLine() method. We iterate until the BufferedReader return null
		 * which means there's no more data to read. Each line will appended to a StringBuilder and returned as String.
		 */
		BufferedReader reader = new BufferedReader(new InputStreamReader(is));
		StringBuilder sb = new StringBuilder();

		String line = null;
		try {
			while ((line = reader.readLine()) != null) {
				sb.append(line + "\n");
			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				is.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		return sb.toString();
	}

	public static IErlElement getCurrentSelectedEditorElement(ErlangEditor editor, ITextSelection selection) {
		FileEditorInput input = (FileEditorInput) editor.getEditorInput();
		IErlModule module = null;
		try {
			module = ModelUtils.getModuleFromExternalModulePath(input.getFile().getName());
		} catch (ErlModelException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		if (module == null) {
			return null;
		}
       
		try {
			return module.getElementAt(selection.getOffset());
		} catch (ErlModelException e) {
			return null;
		}
	}

	public static ErlangEditor getActiveErlangEditor() {
		IWorkbench workbench = Activator.getDefault().getWorkbench();
		IWorkbenchPage page = workbench.getActiveWorkbenchWindow().getActivePage();
		IEditorPart editorPart = page.getActiveEditor();

		if (editorPart == null || !(editorPart instanceof ErlangEditor)) {
			return null;
		}

		return (ErlangEditor) editorPart;
	}
}
