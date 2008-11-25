package mroi.common;

public interface Readable {
	public Readable read(String s) throws Exception;
	public Readable read(LispToLists.LispNode s) throws Exception; 
}
