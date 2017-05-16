/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JPanel;
import story.Story;
import story.Template;

/**
 *
 * @author santi
 */
public class TemplateSequencePanel extends JPanel {
    static final int PHASE_REGION_PADDING = 40;
    
    Story story = null;
    int m_vp_x = 0;
    int m_vp_y = 0;
    double m_vp_zoom = 1.0;
    HashMap<Template,Location> m_locations = new HashMap<Template,Location>();
    List<Template> selected = new LinkedList<Template>();
    Template mouseOver = null;
    int last_mouse_x = -1, last_mouse_y = -1;
    StoryEditorPanel parent = null;
    List<Integer> phase_thresholds = new LinkedList<Integer>();
    
    
    public TemplateSequencePanel(Story s, StoryEditorPanel p) {
        parent = p;
        addMouseMotionListener(new TemplateSequencePanelMouseListener(this));
        addMouseListener(new TemplateSequencePanelMouseListener(this));

        reset(s);
    }
    
    
    public void reset(Story s) {
        story = s;
        m_locations.clear();
        phase_thresholds.clear();
        selected.clear();

        for(Template t:story.getTemplates()) {
            addTemplate(t);
        }
        
        updateLocations();
    }
    
    
    public void updateLocations() {
        for(Template t:story.getTemplates()) {
            if (!m_locations.containsKey(t)) addTemplate(t);
        }
        
        List<Template> toDelete = new LinkedList<Template>();
        for(Template t:m_locations.keySet()) {
            if (!story.getTemplates().contains(t)) toDelete.add(t);
        }
        for(Template t:toDelete) {
            m_locations.remove(t);
            selected.remove(t);
        }
        
        mouseOver = null;
    }
    
    
    public void addTemplate(Template t) {
        int maxx = 0;
        boolean first = true;
        for(Template t2:m_locations.keySet()) {
            if (t2.phase==t.phase) {
                Location l2 = m_locations.get(t2);
                if (first) {
                    maxx = l2.m_x;
                    first = false;
                } else {
                    if (l2.m_x > maxx) maxx = l2.m_x;
                }
            }
        }
        
        // find the proper phase region:
        int phase = t.phase;
        int miny = -1, maxy = -1;
        if (phase<=0) {
            phase = 0;
            t.phase = 0;
        }
        while (phase_thresholds.size()<phase+2) {
            if (phase_thresholds.isEmpty()) {
                phase_thresholds.add(0);
            } else {
                phase_thresholds.add(phase_thresholds.get(phase_thresholds.size()-1)+PHASE_REGION_PADDING*2);
            }
        }
        miny = phase_thresholds.get(phase);
        maxy = phase_thresholds.get(phase+1);
        
        
        m_locations.put(t,new Location(maxx + 64, (miny+maxy)/2, t, this));
        
        mouseOver = null;
    }
    
    
    public void recomputePhases() {
        // determine if any template is out of its phase region:
        for(Template t:m_locations.keySet()) {
            Location l = m_locations.get(t);
            
            int miny = phase_thresholds.get(t.phase);
            int maxy = phase_thresholds.get(t.phase+1);
            
            if (l.m_y<miny-PHASE_REGION_PADDING) {
//                System.out.println("recomputePhases: case 1");
                // template moved to a previous phase:
                if (t.phase==0) {
                    phase_thresholds.add(0,l.m_y-PHASE_REGION_PADDING);
                    for(Template t2:m_locations.keySet()) {
                        if (t2!=t) t2.phase++;
                    }
                } else {
                    t.phase--;
                    recomputePhases();
                    return;
                }
            
            } else if (l.m_y<miny+PHASE_REGION_PADDING) {
//                System.out.println("recomputePhases: case 2");
                // Template pushing the minimum border of a phase:
                int amount = (miny+PHASE_REGION_PADDING) - l.m_y;
                for(int i = 0;i<=t.phase;i++) {
                    phase_thresholds.set(i,phase_thresholds.get(i)-amount);
                }
                for(Template t2:m_locations.keySet()) {
                    Location l2 = m_locations.get(t2);
                    if (t2.phase<t.phase && l2.m_y>(miny+PHASE_REGION_PADDING)-amount) {
                        l2.m_y-=amount;
                    }
                }

            } else if (l.m_y>maxy+PHASE_REGION_PADDING) {
//                System.out.println("recomputePhases: case 3");
                // Template moved to a later phase:
                if (t.phase==phase_thresholds.size()-2) {
                    phase_thresholds.add(l.m_y+PHASE_REGION_PADDING);
                    t.phase++;
                } else {
                    t.phase++;
                    recomputePhases();
                    return;
                }

            } else if (l.m_y>maxy-PHASE_REGION_PADDING) {
//                System.out.println("recomputePhases: case 4");
                // Template pushing the maximum border of a phase:
                int amount = l.m_y - (maxy-PHASE_REGION_PADDING);
                for(int i = t.phase+1;i<phase_thresholds.size();i++) {
                    phase_thresholds.set(i,phase_thresholds.get(i)+amount);
                }
                for(Template t2:m_locations.keySet()) {
                    if (t2.phase>t.phase) m_locations.get(t2).m_y+=amount;
                }
            
            }
        }
        
        // adjust the first and last thresholds:
        boolean first = true;
        int miny = 0;
        int maxy = 0;
        int minphase = 0;
        for(Template t:m_locations.keySet()) {
            if (first) {
                miny = maxy = m_locations.get(t).m_y;
                minphase = t.phase;
                first = false;
            } else {
                if (m_locations.get(t).m_y<miny) miny = m_locations.get(t).m_y;
                if (m_locations.get(t).m_y>maxy) maxy = m_locations.get(t).m_y;
                if (t.phase<minphase) minphase = t.phase;
            }
        }
//        System.out.println("recomputePhases: minphase = " + minphase);

        
        if (phase_thresholds.size()>1) {
            if (minphase==0) {
                phase_thresholds.set(0,miny-PHASE_REGION_PADDING);

                // Checking if there is any extra phases at the beginning:
                while (phase_thresholds.get(0)>=phase_thresholds.get(1)) {
                    phase_thresholds.remove(1);
                    for(Template t:m_locations.keySet()) t.phase--;
                }
            } else {
                phase_thresholds.set(0,miny-PHASE_REGION_PADDING*3);
                phase_thresholds.set(1,miny-PHASE_REGION_PADDING);
 
                // Checking if there is any extra phases at the beginning:
                while (phase_thresholds.get(0)>=phase_thresholds.get(1)) {
                    phase_thresholds.remove(1);
                    for(Template t:m_locations.keySet()) t.phase--;
                }                
            }

            phase_thresholds.set(phase_thresholds.size()-1,maxy+PHASE_REGION_PADDING);
            // Checking if there is any extra phases at the end:
            while (phase_thresholds.get(phase_thresholds.size()-1)<=phase_thresholds.get(phase_thresholds.size()-2)) {
                phase_thresholds.remove(phase_thresholds.size()-2);
            }
            
            // check for any unused phases in the middle:
            for(int i = 0;i<phase_thresholds.size()-1;i++) {
                if (phase_thresholds.get(i)>=phase_thresholds.get(i+1)) {
                    phase_thresholds.remove(i+1);
                    for(Template t:m_locations.keySet()) {
                        if (t.phase>=i+1) t.phase--;
                    }
                    recomputePhases();
                    return;
                }
            }
        }
    }

    
    
    public List<Template> getSelected() {
        return selected;
    }
    
    
    public void deselect() {
        selected.clear();
    }
    
    
    public Rectangle getBoundingBox() {
        Rectangle r = null;
        
        for(Location l:m_locations.values()) {
            Rectangle lr = l.boundingBox();
            if (r==null) {
                r = lr;
            } else {
                r.add(lr);
            }
        }
                
        if (r==null) r = new Rectangle(0, 0, 0, 0);
        if (!phase_thresholds.isEmpty()) {
            r.add(r.x, phase_thresholds.get(0));
            r.add(r.x, phase_thresholds.get(phase_thresholds.size()-1));
        }
        return r;
    }
    
    
    public void recomputeViewPort() {
        Rectangle r = getBoundingBox();
        int padding = 32;
        
        r.x-=padding;
        r.y-=padding;
        r.width+=padding*2;
        r.height+=padding*2;
        
        m_vp_x = r.x;
        m_vp_y = r.y;

        if (r.width==0 || r.height==0) m_vp_zoom = 1.0;
        
        double zoom_x = getWidth() / (double)r.width;
        double zoom_y = getHeight() / (double)r.height;
        
        m_vp_zoom = Math.min(zoom_x,zoom_y);
    }
    
    
    
    public void paint(Graphics g) {
        super.paint(g);
        
        g.setColor(Color.WHITE);
        g.fillRect(0,0,getWidth(),getHeight());
        g.setColor(Color.BLACK);
        g.drawRect(0,0,getWidth()-1,getHeight()-1);
               
        recomputeViewPort();      
        
        // draw phase thresholds:
        int phase = 0;
        Rectangle r = getBoundingBox();
        for(Integer i:phase_thresholds) {
            g.setColor(Color.LIGHT_GRAY);
            g.drawLine(toScreenX(r.x), toScreenY(i), toScreenX(r.x+r.width), toScreenY(i));
            if (phase==0) {
                g.drawString("Common",toScreenX(r.x), toScreenY(i)+16);                
            } else {
                g.drawString("Phase " + phase,toScreenX(r.x), toScreenY(i)+16);
            }
            phase++;
        }
        
        
        // Draw arrows:
        for(Location l1:m_locations.values()) {
            for(Location l2:m_locations.values()) {
                if (story.sequenceP(l1.getTemplate(),l2.getTemplate())) {
                    l1.drawLink(l2, g);
                }
            }
        }                
        
        // draw nodes:
        for(Location l:m_locations.values()) {
            if (mouseOver == l.getTemplate()) {
                l.draw(g,Color.RED);
            } else if (selected.contains(l.getTemplate())) {
                l.draw(g,Color.CYAN);                
            } else {
                l.draw(g,null);
            }      
        }       
    }
    
    
    public void mouseMoved(int x,int y) {
        double rx = fromScreenX(x);
        double ry = fromScreenY(y);
        
        Template oldMouseOver = mouseOver;
        
        last_mouse_x = x;
        last_mouse_y = y;
        
        mouseOver = null;
        for(Location l:m_locations.values()) {
            if (l.point_inside(rx, ry)) mouseOver = l.getTemplate();
        }
        
        if (mouseOver!=oldMouseOver) {
            parent.refreshHighlights();
            parent.repaint();
        }
        
//        System.out.println("Mouse move to: " + x + "," + y + " -> " + rx + "," + ry);
    }
   
    
    public void mouseDragged(int x,int y) {
        double rx = fromScreenX(x);
        double ry = fromScreenY(y);
        
        double previous_rx = fromScreenX(last_mouse_x);
        double previous_ry = fromScreenY(last_mouse_y);
        
        last_mouse_x = x;
        last_mouse_y = y;        
        
        if (mouseOver!=null) {
            Location l = m_locations.get(mouseOver);
            l.m_x += rx - previous_rx;
            l.m_y += ry - previous_ry;
                        
//            System.out.println("Location moved by: " + (rx - previous_rx) + "," + (ry - previous_ry));
            parent.repaint();
        }

//        System.out.println("Mouse drag to: " + x + "," + y + " -> " + rx + "," + ry);        
    }
    
    

    public void mouseClicked(int x,int y, int button) {
        double rx = fromScreenX(x);
        double ry = fromScreenY(y);
        
        last_mouse_x = x;
        last_mouse_y = y;
        
        if (mouseOver!=null) {
            if (button==1) {
                // Left button selects only one template:
                selected.clear();
                selected.add(mouseOver);
                parent.refreshHighlights();
                parent.repaint();                
            } else {
                // right button toogles selection:
                if (selected.contains(mouseOver)) {
                    selected.remove(mouseOver);
                    parent.refreshHighlights();
                    parent.repaint();
                } else {
                    selected.add(mouseOver);
                    parent.refreshHighlights();
                    parent.repaint();
                }
                
            }
        }
        
//        System.out.println("Mouse click (" + button + ") at: " + x + "," + y + " -> " + rx + "," + ry);
    }    
    
    
    public int toScreenX(double x) {
        return (int) (((x - m_vp_x) * m_vp_zoom));
    }

    public int toScreenY(double y) {
        return (int) (((y - m_vp_y) * m_vp_zoom));
    }

    public int toScreen(double v) {
        return (int) (v * m_vp_zoom);
    }

    public double fromScreenX(double x) {
        return (int) ((x / m_vp_zoom) + m_vp_x);
    }

    public double fromScreenY(double y) {
        return (int) ((y / m_vp_zoom) + m_vp_y);
    }

    public double fromScreen(double v) {
        return (int) (v / m_vp_zoom);
    }
    
}
