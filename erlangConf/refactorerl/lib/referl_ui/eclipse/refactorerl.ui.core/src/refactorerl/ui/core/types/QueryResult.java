package refactorerl.ui.core.types;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class QueryResult {
	private String file;
	private int startPos;
	private int endPos;
	private String value;
	private QueryResultType type; 
	
	private ArrayList<QueryResult> elements = new ArrayList<QueryResult>();
	
	public enum QueryResultType { NOPOS, VALID_ELEMENT, ERROR }
	
	public QueryResult(String value) {
		this.value = value;
		type = QueryResultType.NOPOS;
	}
	
	public QueryResult(String file, int startPos, int endPos, String value) {
		this.file = file;
		this.startPos = startPos;
		this.endPos = endPos;
		this.value = value;
		type = QueryResultType.VALID_ELEMENT;
	}
	
	public QueryResultType getType() {
		return type;
	}
	
	public String getValue() {
		return value;
	}
	
	public int getStartPos() {
		return this.startPos;
	}
	
	public int getEndPos() {
		return this.endPos;
	}
	
	public String getFile() {
		return file;
	}
	
	public void add(QueryResult result) {
		elements.add(result);
	}
	
	public Collection<QueryResult> getElements() {
		return Collections.unmodifiableList(elements);
	}
	
	@Override
	public String toString() {
		return value;
	}
}
