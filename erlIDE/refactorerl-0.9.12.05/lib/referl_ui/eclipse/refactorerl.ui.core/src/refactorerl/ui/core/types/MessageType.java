package refactorerl.ui.core.types;

public enum MessageType {
	result, saved, renamed, other;
	
	public static MessageType getType(String type) {
		if (type.equalsIgnoreCase("result")) {
			return result;
		} else if (type.equalsIgnoreCase("saved")) {
			return saved;
		} else if (type.equalsIgnoreCase("renamed")) {
			return renamed;
		}
		
		return other;
	}
}
