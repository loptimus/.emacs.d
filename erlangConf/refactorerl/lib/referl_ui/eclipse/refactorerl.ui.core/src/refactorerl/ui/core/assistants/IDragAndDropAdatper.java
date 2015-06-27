package refactorerl.ui.core.assistants;

import org.eclipse.swt.dnd.TransferData;

public interface IDragAndDropAdatper {

	public boolean validateDrop(String transformName, Object target, int operation, TransferData transferType);
}
