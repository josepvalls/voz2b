/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package story;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author santi
 */
public class Entity extends CUDObject {
    public static List<Entity> predefinedEntities = new LinkedList<Entity>();
    public static String[] predefinedEntityNames;
    static {
        reloadFromDisk();
    }
    
    
    public Entity m_parent = null;
    
    public Entity() {
        m_id = null;
        m_parent = null;
    }    
    
    public Entity(String name, Entity parent) {
        m_id = name;
        m_parent = parent;
    }
    
    public String toString() {
        if (m_parent == null) {
            return "(" + m_id + ")";
        }
        return "(" + m_id + " :type " + m_parent.m_id + ")";
    }
    
    public boolean equals(Object o) {
        if (o instanceof Entity) {
            Entity e = (Entity)o;
            if (e.m_id.equals(m_id)) {
                if (m_parent==null && e.m_parent==null) return true;
                if (m_parent!=null && e.m_parent!=null) return m_parent.equals(e.m_parent);
            } 
        } 
        return false;
    }  
    
    public static Entity newEntity(String name,String parent,Story s) {
        Entity pe = null;
        if (parent!=null) {
            for(Entity e:predefinedEntities) if (e!=null && e.m_id.equals(parent)) pe = e;
            if (pe==null) pe = s.getEntity(parent);
            if (pe==null) {
                // create a temporary parent entity:
                pe = new Entity(parent,null);
            }
        }
        Entity tmp = s.getEntity(name);
        if (tmp!=null) {
            // entity already existed, updating it!
            tmp.m_parent = pe;
            return tmp;
        } else {
            return new Entity(name,pe);
        }
    }
    
    public static void reloadFromDisk() {
        try {
            List<Entity> tmp_predefinedEntities = new LinkedList<Entity>();
            String[] tmp_predefinedEntityNames;
            
            
            HashMap<String,Entity> dictionary = new HashMap<String,Entity>(); 
            BufferedReader br = new BufferedReader(new FileReader("entities.txt"));
            String line;
            do{
                line = br.readLine();
                if (line!=null) {
                    StringTokenizer st = new StringTokenizer(line,",");
                    List<String> tokens = new LinkedList<String>();
                    while(st.hasMoreTokens()) tokens.add(st.nextToken().trim());
                    if (tokens.size()==0) {
                        tmp_predefinedEntities.add(null);
                        dictionary.put(null,null);
                    } else if (tokens.size()==1) {
                        Entity e = new Entity(tokens.get(0),null);
                        tmp_predefinedEntities.add(e);
                        dictionary.put(tokens.get(0),e);
                    } else {
                        Entity parent = dictionary.get(tokens.get(1));
                        if (parent==null) throw new Exception("Entity " + tokens.get(1) + " is undefined!");
                        Entity e = new Entity(tokens.get(0),parent);
                        tmp_predefinedEntities.add(e);
                        dictionary.put(tokens.get(0),e);
                    }
                }                
            }while(line!=null);
            
            tmp_predefinedEntityNames = new String[tmp_predefinedEntities.size()];
            tmp_predefinedEntityNames[0] = "<none>";
            for(int i = 1;i<tmp_predefinedEntities.size();i++) {
                tmp_predefinedEntityNames[i] = tmp_predefinedEntities.get(i).m_id;
            }        
            
            predefinedEntities = tmp_predefinedEntities;
            predefinedEntityNames = tmp_predefinedEntityNames;
            
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }        
    }
            
}
