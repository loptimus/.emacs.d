package refactorerl.ui.core.utils;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

import refactorerl.ui.core.constants.ReferlConstants;

/**
 * Common, convenience functions related to the workspace and the workbecnch
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */
public final class WorkspaceUtils {

	/**
	 * converts a relative IPath into a absolute path string which prefix is the workspace directory
	 * 
	 * @param path
	 *            relative path
	 * @return absolute path in the system
	 */
	public static String pathToString(IPath path) {
		StringBuffer buffer = new StringBuffer();
		IPath root=ResourcesPlugin.getWorkspace().getRoot().getLocation();
		//root = root.setDevice(root.getDevice().toLowerCase());
		buffer.append(root.toPortableString());
		buffer.append(path.toPortableString());
		return buffer.toString();
	}

	/**
	 * finds the file element in the workspace which absolute path is the parameter
	 * 
	 * @param path
	 *            absolute path of the file
	 * @return Resource at the path
	 */
	public static IFile pathToResource(String path) {

		if (path == null || path.equals("")) {
			throw new IllegalArgumentException("Path parameter is required");
		}

		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		String location = root.getLocation().toPortableString();

		if (!path.startsWith(location)) {
			throw new IllegalArgumentException("The file is not in the workspace: " + path);
		}

		String fileLocation = path.substring(location.length() + 1);
		String[] resources = fileLocation.split("/");

		IContainer container = root.getProject(resources[0]);
		for (int i = 1; i < resources.length - 1; i++) {
			if (container instanceof IProject) {
				container = ((IProject) container).getFolder(resources[i]);
			} else {
				container = ((IFolder) container).getFolder(resources[i]);
			}
		}

		String fileName = resources[resources.length - 1];
		if (container instanceof IProject) {
			return ((IProject) container).getFile(fileName);
		} else {
			return ((IFolder) container).getFile(fileName);
		}
	}

	/**
	 * 
	 * @param path
	 * @throws CoreException
	 */
	public static void refreshWorkspace(IPath path) throws CoreException {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IResource resource = root.findMember(path.makeRelativeTo(root.getLocation()));
		
		//IResource[] res = root.members();
		if (resource != null) {
			resource.refreshLocal(IResource.DEPTH_INFINITE, null);
		}
	}
	public static void refreshWorkspace() throws CoreException{
		 ResourcesPlugin.getWorkspace().getRoot().refreshLocal(IResource.DEPTH_INFINITE, null);
	}

	/**
	 * finds all of the projects in the workspace which has <b>org.erlide.core.erlnature</b> nature
	 * 
	 * @return Erlang projects in the worksapce
	 * @throws CoreException
	 */
	public static List<IProject> getErlangProjects() throws CoreException {
		List<IProject> projects = new ArrayList<IProject>();
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		for (IProject project : root.getProjects()) {
			if (project.hasNature(ReferlConstants.ERL_NATURE)) {
				projects.add(project);
			}
		}

		return projects;
	}

}
