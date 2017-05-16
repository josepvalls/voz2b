/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package story;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

/**
 *
 * @author santi
 */
public class Expression extends CUDObject {
    public static HashMap<String,Integer> predefinedExpressions = new HashMap<String,Integer>();
    public static String[] predefinedExpressionsNames;
    public static List<String> predefinedExpressionsNamesList = new LinkedList<String>();
    static {
        reloadFromDisk();
    }
        
    static int next_ID = 0;
    public String m_head = null;
    public List<CUDObject> m_children = new LinkedList<CUDObject>();
    
    public Expression() {
        m_id = "E" + (next_ID++);
        m_head = null;
    }
    
    public Expression(String head) {
        m_id = "E" + (next_ID++);
        m_head = head;
    }

    public Expression(String head, CUDObject p1) {
        m_id = "E" + (next_ID++);
        m_head = head;
        m_children.add(p1);
    }

    public Expression(String head, CUDObject p1, CUDObject p2) {
        m_id = "E" + (next_ID++);
        m_head = head;
        m_children.add(p1);
        m_children.add(p2);
    }

    public Expression(String head, CUDObject p1, CUDObject p2, CUDObject p3) {
        m_id = "E" + (next_ID++);
        m_head = head;
        m_children.add(p1);
        m_children.add(p2);
        m_children.add(p3);
    }

    public Expression(String head, List<CUDObject> pl) {
        m_id = "T" + (next_ID++);
        m_head = head;
        m_children.addAll(pl);
    }
    
    public Expression(String ID, String head, List<CUDObject> pl) {
        m_id = ID;
        
        // check which is the next_ID that need to be set:
        if (m_id.charAt(0)=='E') {
            String num = m_id.substring(1);
            try {
                int i = Integer.parseInt(num);
                next_ID = i+1; 
            } catch(NumberFormatException ex) {                
            }
        }
        m_head = head;
        m_children.addAll(pl);
    }
    
    public List<CUDObject> recursiveChildren() {
        List<CUDObject> l = new LinkedList<CUDObject>();
        for(CUDObject o:m_children) {
            l.add(o);
            if (o instanceof Expression) l.addAll(((Expression)o).recursiveChildren());
        }
        return l;
    }
    
    public String toString() {
        String tmp = "(" + m_head;
        for(CUDObject o:m_children) {
            tmp += " " + o.m_id;
        }
        return "(" + tmp + ") :name " + m_id + ")";
    }    
    
    
    public static void reloadFromDisk() {
        try {
            HashMap<String,Integer> tmp_predefinedExpressions = new HashMap<String,Integer>();
            String[] tmp_predefinedExpressionsNames;
            List<String> tmp_predefinedExpressionsNamesList = new LinkedList<String>();
            
            BufferedReader br = new BufferedReader(new FileReader("expressions.txt"));
            String line;
            do{
                line = br.readLine();
                if (line!=null) {
                    StringTokenizer st = new StringTokenizer(line,",");
                    List<String> tokens = new LinkedList<String>();
                    while(st.hasMoreTokens()) tokens.add(st.nextToken().trim());
                    if (tokens.size()==2) {
                        int n = Integer.parseInt(tokens.get(1));
                        tmp_predefinedExpressions.put(tokens.get(0),n);
                    } else {
                        throw new Exception("Incorrect expression definition line: '" + line + "'");
                    }
                }                
            }while(line!=null);
            
            tmp_predefinedExpressionsNames = new String[tmp_predefinedExpressions.keySet().size()+1];

            int i = 0;
            tmp_predefinedExpressionsNames[i++]="<none>";
            for(String e:tmp_predefinedExpressions.keySet()) tmp_predefinedExpressionsNamesList.add(e);
            // sort them alphabetically:
            Collections.sort(tmp_predefinedExpressionsNamesList);
            for(String e:tmp_predefinedExpressionsNamesList) tmp_predefinedExpressionsNames[i++]=e;
            
            predefinedExpressions = tmp_predefinedExpressions;
            predefinedExpressionsNames = tmp_predefinedExpressionsNames;
            predefinedExpressionsNamesList = tmp_predefinedExpressionsNamesList;
            
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }          
    }
}
