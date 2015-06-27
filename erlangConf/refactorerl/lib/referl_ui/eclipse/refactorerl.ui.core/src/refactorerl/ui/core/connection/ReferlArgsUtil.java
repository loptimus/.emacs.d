package refactorerl.ui.core.connection;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import refactorerl.ui.core.constants.ReferlArgsConstants;
import refactorerl.ui.core.types.RefactorerlFunlist;
import refactorerl.ui.core.types.RefactorerlKeyValuePair;
import refactorerl.ui.core.types.RefactorerlMaclist;
import refactorerl.ui.core.types.RefactorerlOrderList;
import refactorerl.ui.core.types.RefactorerlPosrange;
import refactorerl.ui.core.types.RefactorerlReclist;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * This class has convenience functions to convert java object into erlang object back and force. This is the only part of the java application
 * where there are operations over Erlang specific types.
 * 
 * 
 * @author zoltan verebes, modified by Horv�th Gergely 
 * 
 */
public class ReferlArgsUtil implements ReferlArgsConstants {	
	private static HashMap<String, Constructor<? extends OtpErlangObject>> typeMapping = new HashMap<String, Constructor<? extends OtpErlangObject>>();
	
	static {
		try {
			typeMapping.put(ARITY, OtpErlangInt.class.getConstructor(int.class));
			typeMapping.put(FILE, OtpErlangString.class.getConstructor(String.class));
			typeMapping.put(FILENAME, OtpErlangString.class.getConstructor(String.class));
			typeMapping.put(FUNCTION, OtpErlangAtom.class.getConstructor(String.class));
			typeMapping.put(FUNLIST, RefactorerlFunlist.class.getConstructor(List.class));
			typeMapping.put(MACLIST, RefactorerlMaclist.class.getConstructor(List.class));
			typeMapping.put(MACNAME, OtpErlangString.class.getConstructor(String.class));
			typeMapping.put(MACRO, OtpErlangString.class.getConstructor(String.class));
			typeMapping.put(MISSING, OtpErlangAtom.class.getConstructor(String.class));
			typeMapping.put(MODULE, OtpErlangAtom.class.getConstructor(String.class));
			typeMapping.put(NAME, OtpErlangString.class.getConstructor(String.class));
			typeMapping.put(NUMBER, OtpErlangInt.class.getConstructor(int.class));
			typeMapping.put(ORDER, RefactorerlOrderList.class.getConstructor(List.class));
			typeMapping.put(POSITION, OtpErlangInt.class.getConstructor(int.class));
			typeMapping.put(POSRANGE, RefactorerlPosrange.class.getConstructor(int[].class));
			typeMapping.put(RECORD, OtpErlangAtom.class.getConstructor(String.class));
			typeMapping.put(RECLIST, RefactorerlReclist.class.getConstructor(List.class));
			typeMapping.put(TEXT, OtpErlangString.class.getConstructor(String.class));
			typeMapping.put(VARNAME, OtpErlangString.class.getConstructor(String.class));
			typeMapping.put(QUERYSTR, OtpErlangString.class.getConstructor(String.class));
		} catch (SecurityException e) {
			throw new IllegalArgumentException("Could not create type mapping", e);
		} catch (NoSuchMethodException e) {
			throw new IllegalArgumentException("Could not create type mapping", e);
		}
	}

	public static OtpErlangList mapToArglist(Map<String, Object> map) {
		List<OtpErlangObject> result = new ArrayList<OtpErlangObject>();
		
		for (String key : map.keySet()) {
			Constructor<? extends OtpErlangObject> constructor = typeMapping.get(key);
			
			try {
				result.add(new RefactorerlKeyValuePair(key, constructor.newInstance(map.get(key))));
				//result.add(constructor.newInstance(map.get(key)));
			} catch (IllegalArgumentException e) {
				throw new IllegalArgumentException("Could not convert map to erlang arglist", e);
			} catch (InstantiationException e) {
				throw new IllegalArgumentException("Could not convert map to erlang arglist", e);
			} catch (IllegalAccessException e) {
				throw new IllegalArgumentException("Could not convert map to erlang arglist", e);
			} catch (InvocationTargetException e) {
				throw new IllegalArgumentException("Could not convert map to erlang arglist", e);
			}
		}
		return new OtpErlangList(result.toArray(new OtpErlangObject[0]));
	}
}
