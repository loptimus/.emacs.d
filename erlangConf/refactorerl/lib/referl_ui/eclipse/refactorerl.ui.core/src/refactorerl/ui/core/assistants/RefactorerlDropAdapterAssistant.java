package refactorerl.ui.core.assistants;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.util.LocalSelectionTransfer;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.navigator.CommonDropAdapter;
import org.eclipse.ui.navigator.CommonDropAdapterAssistant;

import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.extensionpoints.ContextMenuDragAndDropsExtensionPointProxy;
import refactorerl.ui.core.extensionpoints.ExtensionPointParser;

/**
 * Common Drag and drop assistant to check which drag and drops are available for the current prameters of any event. When any of them is valid
 * the handleDrop gets called after dragging of the element finished.
 * 
 * Handle a drop event means in the current context to call a Refactorerl function on Referl_ui module. It is done by the handleDrop function of
 * this class
 * 
 * @author zoltan verebes
 * 
 */
public class RefactorerlDropAdapterAssistant extends CommonDropAdapterAssistant {

	private List<ContextMenuDragAndDropsExtensionPointProxy> extensions = ExtensionPointParser.getInstance()
			.findContextMenuDragAndDropsExtensionPointProxyExtensions();

	private List<ContextMenuDragAndDropsExtensionPointProxy> validDragAndDrops = new ArrayList<ContextMenuDragAndDropsExtensionPointProxy>();

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.navigator.CommonDropAdapterAssistant#handleDrop(org.eclipse .ui.navigator.CommonDropAdapter,
	 * org.eclipse.swt.dnd.DropTargetEvent, java.lang.Object)
	 */
	@Override
	public IStatus handleDrop(CommonDropAdapter dropAdapter, DropTargetEvent dropTargetEvent, Object target) {
		IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService(IHandlerService.class);

		for (ContextMenuDragAndDropsExtensionPointProxy extension : validDragAndDrops) {
			try {
				handlerService.executeCommand(extension.getCommandId(), new DragAndDropEvent(extension.getTransformName(), dropAdapter,
						dropTargetEvent, target));
			} catch (Exception e) {
				throw new ErlangException("Could not finish drag and drop due to occoured exception", e);
			}

		}

		return Status.OK_STATUS;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.navigator.CommonDropAdapterAssistant#validateDrop(java .lang.Object, int, org.eclipse.swt.dnd.TransferData)
	 */
	@Override
	public IStatus validateDrop(Object target, int operation, TransferData transferType) {

		if (!LocalSelectionTransfer.getTransfer().isSupportedType(transferType)) {
			return Status.CANCEL_STATUS;
		}

		try {
			// a drag and drop is valid if it is defined in the plugin.xml and it's valid for the parameters of the event
			return (getValidExtensionPoints(target, operation, transferType).size() == 0 ? Status.CANCEL_STATUS : Status.OK_STATUS);
		} catch (ExecutionException e) {
			return Status.CANCEL_STATUS;
		}
	}

	/**
	 * a drag and drop is valid if any of the current available drag and drop events is valid. To check it a list of possible Drag and Drop need
	 * to be gathered. Drag and drops are defined in the plugin.xml
	 * 
	 * @param target
	 * @param operation
	 * @param transferType
	 * @return drag and drops which passed the validation with the parameters of the event
	 * @throws ExecutionException
	 */
	private List<ContextMenuDragAndDropsExtensionPointProxy> getValidExtensionPoints(Object target, int operation, TransferData transferType)
			throws ExecutionException {
		validDragAndDrops = new ArrayList<ContextMenuDragAndDropsExtensionPointProxy>();
		for (ContextMenuDragAndDropsExtensionPointProxy extension : extensions) {
			IDragAndDropAdatper adapter = extension.getAdapter();
			if (adapter.validateDrop(extension.getTransformName(), target, operation, transferType)) {
				validDragAndDrops.add(extension);
			}
		}

		return validDragAndDrops;
	}

}
