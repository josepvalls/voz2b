/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package story;

import java.util.LinkedList;
import java.util.List;

/**
 *
 * @author santi
 */
public class Template {
    public String ID = "";
    public String text = "";
    public int phase = 1;
    
    public List<HUDCUDLink> links = new LinkedList<HUDCUDLink>();
    
    public Template(String a_ID, String a_text) {
        ID = a_ID;
        text = a_text;
    }
    
    public Template(String a_ID, String a_text, int p) {
        ID = a_ID;
        text = a_text;
        phase = p;
    }
    
    public void removeLink(HUDCUDLink l) {
        
        links.remove(l);

        // Check if this was a child of an expression and delete it from there:
        for(HUDCUDLink l2:links) {
            if (l2.m_start<=l.m_start && l2.m_end>=l.m_end) {
                if (l2.m_object instanceof Expression) {
                    Expression exp = (Expression)l2.m_object;
                    exp.m_children.remove(l.m_object);
                }
            }
        }
        
    }
    
    
    public HUDCUDLink newLink(int start, int end, CUDObject o) {
        if (start>=end) return null;
        // verify that it does not PARTIALLY overlap with another link:
        for(HUDCUDLink l:links) {
            if (l.m_start<start) {
                if (l.m_end>start && l.m_end<end) return null;
            } else if (l.m_start==start) {
            } else if (l.m_start>start && l.m_start<end) {
                if (l.m_end>end) return null;
            } else if (l.m_start==end) {
            } else {
            }
        }
        HUDCUDLink l = new HUDCUDLink(this, start, end, o);
        
        
        // Check if this is a child of an expression and add it:
        for(HUDCUDLink l2:links) {
            if (l2.m_start<=l.m_start && l2.m_end>=l.m_end) {
                if (l2.m_object instanceof Expression) {
                    // Only if there is no intermediate expression:                
                    Expression exp = (Expression)l2.m_object;
                    HUDCUDLink intermediate = null;
                    for(HUDCUDLink l3:links) {
                        if (l3!=l2 && 
                            l3.m_start>=l2.m_start && l3.m_end<=l3.m_end &&
                            l3.m_start<=l.m_start && l3.m_end>=l.m_end) {
                            intermediate = l3;
                            break;
                        }
                    }
                    if (intermediate==null) {
                        exp.m_children.add(l.m_object);
                        
                        // See if there is any children that has to be deleted as a consequence:
                        for(HUDCUDLink l3:links) {
                            if (exp.m_children.contains(l3.m_object) &&
                                l3.m_start>=l.m_start && l3.m_end<=l.m_end) {
                                exp.m_children.remove(l3.m_object);
                            }
                        }
                    }
                }
            }
        }
        
        // Check if there are any already existing child expressions:
        // Check if this is a child of an expression and add it:
        if (l.m_object instanceof Expression) {
            Expression exp = (Expression)l.m_object;
            for(HUDCUDLink l2:links) {
                if (l.m_start<=l2.m_start && l.m_end>=l2.m_end) {
                    // Only if there is no intermediate expression/entity                
                    HUDCUDLink intermediate = null;
                    for(HUDCUDLink l3:links) {
                        if (l3!=l2 && 
                            l3.m_start>=l.m_start && l3.m_end<=l.m_end &&
                            l3.m_start<=l2.m_start && l3.m_end>=l2.m_end) {
                            intermediate = l3;
                            break;
                        }
                    }                
                    if (intermediate==null) {
                        exp.m_children.add(l2.m_object);

                        // See if there is any children that has to be deleted as a consequence:
                        for(HUDCUDLink l3:links) {
                            if (l3!=l2 && 
                                exp.m_children.contains(l3.m_object) &&
                                l3.m_start>=l2.m_start && l3.m_end<=l2.m_end) {
                                exp.m_children.remove(l3.m_object);
                            }
                        }
                    }
                }
            }
        }
        
        links.add(l);
        
        return l;
    }
    
    
    public boolean containsLinkTo(CUDObject o) {
        for(HUDCUDLink l:links) {
            if (l.m_object==o) return true;
        }
        return false;
    }
    
    public String toString() {
        String tmp = "";
        boolean start_quotes = true;
        
        for(int i = 0;i<text.length();i++) {
            // determine if any link starts here:
            List<HUDCUDLink> endingHere = new LinkedList<HUDCUDLink>();
            List<HUDCUDLink> startingHere = new LinkedList<HUDCUDLink>();
            for(HUDCUDLink l:links) {
                if (l.m_start==i) startingHere.add(l);
                if (l.m_end==i+1) endingHere.add(l);
            }
            
            if (startingHere.isEmpty() && start_quotes) {
                tmp+="\"";
                start_quotes = false;
            }
            if (!startingHere.isEmpty() && !start_quotes) {
                tmp+="\"";
                start_quotes = true;
            }
            
            // starting here:
            while(!startingHere.isEmpty()) {
                // select the one that ends the later:
                HUDCUDLink next = null;
                for(HUDCUDLink l:startingHere) {
                    if (next==null || l.m_end>next.m_end) next = l;
                }
                startingHere.remove(next);
                tmp += " (" + next.m_object.m_id + " ";
                start_quotes = true;
            }
            
            // character:
            if (start_quotes) {
                tmp+="\"";
                start_quotes = false;
            }
            tmp+=text.charAt(i);
            
            if (!endingHere.isEmpty() && !start_quotes) {
                tmp+="\"";
                start_quotes = true;
            }

            while(!endingHere.isEmpty()) {
                HUDCUDLink next = endingHere.remove(0);
                tmp += ") ";
            }
        }
        if (!start_quotes) tmp+="\"";
        
        return "(" + ID + " " + tmp + ")";
    }
}
