package refactorerl.ui.core.assistants;

import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.navigator.CommonDropAdapter;

public class DragAndDropEvent extends Event {

	private CommonDropAdapter adapter;
	private DropTargetEvent targetEvent;
	private Object target;
	private String transformName;

	public DragAndDropEvent(String transformName, CommonDropAdapter adapter,
			DropTargetEvent targetEvent, Object target) {
		this.adapter = adapter;
		this.targetEvent = targetEvent;
		this.target = target;
		this.transformName = transformName;
	}

	public CommonDropAdapter getAdapter() {
		return adapter;
	}

	public DropTargetEvent getTargetEvent() {
		return targetEvent;
	}

	public Object getTarget() {
		return target;
	}
	
	public String getTransformName() {
		return transformName;
	}
}
