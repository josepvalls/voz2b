/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package lispParser;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

/**
 *
 * @author santi
 */
public class LispNode {
    public static final int TYPE_SEQ = 0;
    public static final int TYPE_COMMENT = 1;
    public static final int TYPE_LIST = 2;
    public static final int TYPE_QUOTE = 3;
    public static final int TYPE_SYMBOL = 4;
    public static final int TYPE_STRING = 5;
    
    
    public int type = TYPE_SEQ;
    public String value = null;
    public List<LispNode> children = new LinkedList<LispNode>();
    public LispNode parent = null;
    
    public String toString() {
        switch(type) {
            case TYPE_SEQ:
            {
                String tmp = "";
                for(LispNode c:children) tmp += c + "\n";
                return tmp;
            }
            case TYPE_COMMENT:
                return "; " + value + "\n";
            case TYPE_LIST:
            {
                String tmp = "(\n";
                for(LispNode c:children) tmp += c + "\n";
                return tmp + ")\n";
            }
            case TYPE_QUOTE:
                return "'";
            case TYPE_SYMBOL:
                return value;
            case TYPE_STRING:
                return "\"" + value + "\"";
        }
        return "";
    }
    
    public LispNode(int t, String v, LispNode p) {
        type = t;
        value = v;
        parent = p;
    }
    
    public static LispNode parse(BufferedReader fr) throws IOException {
        if (!fr.markSupported()) return null;
        
        LispNode top = new LispNode(TYPE_SEQ,null,null);
        LispNode current = top;
        
        String token = "";
        int tokenType = 0;
        do{
            // read next token:
            token = "";
            int c = fr.read();
            while(c==' ' || c=='\n' || c=='\t' || c=='\r') {
                if (!fr.ready()) return top;
                c = fr.read();
            }
            if (c=='(') {
                LispNode n = new LispNode(TYPE_LIST,null,current);
                current.children.add(n);
                current = n;
            } else if (c==')') {
                if (current.parent!=null) {
                    current = current.parent;
                } else {
                    System.err.println("LispNode.parse: Extra parenthesis!");
                    return null;
                }
            } else if (c=='\'') {
                LispNode n = new LispNode(TYPE_QUOTE,"'",current);
                current.children.add(n);
            } else if (c=='\"') {
                c = fr.read();
                while(c!='\"') {
                    token += (char)c;
                    c = fr.read();                    
                }
                LispNode n = new LispNode(TYPE_STRING,token,current); 
                current.children.add(n);                
            } else if (c==';') {
                // comments:
                while(c==' ' || c==';') c = fr.read();
                while(c!='\n' && c!='\r') {
                    token += (char)c;
                    c = fr.read();                    
                }
                LispNode n = new LispNode(TYPE_COMMENT,token,current); 
                current.children.add(n);                                
            } else if (c=='#') {
                // block comments:
                fr.mark(8);
                c = fr.read();
                if (c=='|') {
                    // in a block comment:
                    do {
                        c = fr.read();
                        if (c=='|') {
                            c = fr.read();
                            if (c=='#') {
                                // done with comment:
                                LispNode n = new LispNode(TYPE_COMMENT,token,current); 
                                current.children.add(n);  
                                break;
                            } else {
                                token += '|' + (char)c;
                            }
                        } else {
                            token += (char)c;
                        }
                    } while(true);
                } else {
                    fr.reset();
                    parseSymbol(fr,current,'#');
                }
            } else {
                parseSymbol(fr,current,c);
            }            
        }while(fr.ready());
        
        return top;
    }
    
    
    public static void parseSymbol(BufferedReader fr, LispNode current, int c) throws IOException {
        String token = "";
        do {
            token += (char)c;
            fr.mark(8);
            c = fr.read();
        } while(c!=' ' && c!='\n' && c!='\t' && c!='\r' && 
                c!='(' && c!=')' && c!= '\'' && c!='\"' &&
                c!=';');
        fr.reset();
        LispNode n = new LispNode(TYPE_SYMBOL,token,current); 
        current.children.add(n);
    }
    
    
    public LispNode searchSymbol(String v) {
        if (type==TYPE_SYMBOL && value.equals(v)) return this;
        for(LispNode c:children) {
            LispNode tmp = c.searchSymbol(v);
            if (tmp!=null) return tmp;
        }
        return null;
    }

    
    public LispNode searchListStartingBySymbol(String v) {
        if (type==TYPE_SYMBOL && value.equals(v) && 
            parent!=null && parent.children.get(0)==this) return parent;
        for(LispNode c:children) {
            LispNode tmp = c.searchListStartingBySymbol(v);
            if (tmp!=null) return tmp;
        }
        return null;
    }
}
