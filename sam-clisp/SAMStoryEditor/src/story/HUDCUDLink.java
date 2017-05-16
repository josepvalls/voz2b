/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package story;

/**
 *
 * @author santi
 */
public class HUDCUDLink {
    public Template m_template = null;
    public int m_start = 0, m_end = 0;
    public CUDObject m_object = null;
    
    /*
    public HUDCUDLink(Template t, int start, int end) {
        m_template = t;
        m_start = start;
        m_end = end;
    } 
    */
    
    public HUDCUDLink(Template t, int start, int end, CUDObject object) {
        m_template = t;
        m_start = start;
        m_end = end;
        m_object = object;
    } 
    
    
    public String toString() {
        return m_object.getClass().getSimpleName() + "[" + m_start + " - " + m_end + "]";
    }
    
}
