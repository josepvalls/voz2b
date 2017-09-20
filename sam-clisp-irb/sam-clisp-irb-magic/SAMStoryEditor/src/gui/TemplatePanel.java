/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import story.*;

/**
 *
 * @author santi
 */
public class TemplatePanel extends JPanel implements ActionListener, KeyListener {
    Story m_story = null;
    TemplateSequencePanel m_seqEditor = null;
    StoryEditorPanel m_parent = null;
    Template m_template = null;

    int letterx[] = null;
    int letterxend[] = null;
    HashMap<HUDCUDLink,Rectangle2D> linkPositions = new HashMap<HUDCUDLink,Rectangle2D>();
    HUDCUDLink selected = null;
    HUDCUDLink mouseOver = null;
    
    int original_mouse_drag_x = -1;
    int original_mouse_drag_y = -1;
    
    int selected_start = -1;
    int selected_end = -1;
    
    List<Component> buttons = new LinkedList<Component>();
    List<Entity> entitiesListInComboBox = null; // This stores a copy of the list of entities that was generated for the dropbown list to select a parent entity


    public TemplatePanel(Story a_story, TemplateSequencePanel seqEditor, StoryEditorPanel parent) {
        m_story = a_story;
        m_seqEditor = seqEditor;
        m_parent = parent;
        
        addMouseMotionListener(new TemplatePanelMouseListener(this));
        addMouseListener(new TemplatePanelMouseListener(this));
        
        createButtons();
    }
    
    public void reset(Story s) {
        m_story = s;
        m_template = null;
        selected = null;
        letterx = null;
        letterxend = null;
        linkPositions.clear();
        createButtons();
    }
    
    
    public Story getStory() {
        return m_story;
    }
    
    public void createButtons() {

        for(Component b:buttons) {
            this.remove(b);
        }
        buttons.clear();
        
        if (selected!=null) {
            // Delete button:
            JButton b = new JButton("Delete");
            b.setActionCommand("Delete");
            b.addActionListener(this);
            this.add(b);
            buttons.add(b);
            
            if (selected.m_object instanceof Entity) {
                // Select entity name and type:
                Entity e = (Entity)selected.m_object;
                JTextField tf = new JTextField(16);
                if (e.m_id!=null) tf.setText(e.m_id);
                tf.addKeyListener(this);
                this.add(tf);
                buttons.add(tf);
                
                // Select type:
                entitiesListInComboBox = m_story.getAllEntities();
                String []names = new String[entitiesListInComboBox.size()];
                for(int i = 0;i<names.length;i++) {
                    Entity e2 = entitiesListInComboBox.get(i);
                    if (e2==null) names[i] = "<none>";
                             else names[i] = e2.m_id;
                }
                JComboBox cb = new JComboBox(names);
//                JComboBox cb = new JComboBox(m_story.getAllEntityNames());
                int idx = entitiesListInComboBox.indexOf(e.m_parent);
                if (idx!=-1) cb.setSelectedIndex(idx);
                cb.addActionListener(this);
                this.add(cb);
                buttons.add(cb);
            }

            if (selected.m_object instanceof Expression) {
                Expression e = (Expression)selected.m_object;
/*
                // Select expression name, type and parameters:
                // Select entity name and type:
                JTextField tf = new JTextField(16);
                if (e.m_id!=null) tf.setText(e.m_id);
                tf.addKeyListener(this);
                this.add(tf);
                buttons.add(tf);
 */                
                // Select type:
                JComboBox cb = new JComboBox(Expression.predefinedExpressionsNames);
                int idx = Expression.predefinedExpressionsNamesList.indexOf(e.m_head);
                if (idx!=-1) cb.setSelectedIndex(idx+1);
                cb.addActionListener(this);
                this.add(cb);
                buttons.add(cb);
            }
        }
        revalidate();
        m_parent.repaint();
    }


    public void paint(Graphics g) {
        super.paint(g);
/*
        g.setColor(Color.WHITE);
        g.fillRect(0,0,getWidth(),getHeight());
        g.setColor(Color.BLACK);
        g.drawRect(0,0,getWidth()-1,getHeight()-1);
*/
        if (m_seqEditor.getSelected().size()==1) {
            m_template = m_seqEditor.getSelected().get(0);
        } else {
            m_template = null;
            selected = null;
        }

        if (m_template!=null) {
            // draw tempalte, with all the annotations:
            Rectangle2D r = g.getFontMetrics().getStringBounds(m_template.text, g);
            int w = (int)(r.getWidth()+m_template.links.size()*8);
            int h = (int)r.getHeight();
            int start_x = getWidth()/2 - w/2;
            int length = m_template.text.length();
            int letterdepth[] = new int[length];
            int x = start_x;
            int current_depth = 0;
            
            this.setPreferredSize(new Dimension(w, h + 100));            
            

            letterx = new int[length];
            letterxend = new int[length];

            for(int i = 0;i<length;i++) {
                letterdepth[i]=0;
                for(HUDCUDLink l:m_template.links) {
                    if (l.m_start<=i && l.m_end>i) letterdepth[i]++;
                }

                String letter = m_template.text.charAt(i)+"";
                int diff = Math.abs(current_depth - letterdepth[i]);
                Rectangle2D r2 = g.getFontMetrics().getStringBounds(letter, g);
                x+=(diff>1 ? 4+(diff-1)*2:diff*4);
                letterx[i]=x;
                letterxend[i]=(int)(x + r2.getWidth());
                current_depth = letterdepth[i];
                x+=r2.getWidth();
            }

            // draw the boxes:
            linkPositions.clear();
            for(HUDCUDLink l:m_template.links) {
                int depth = 1;
                int depthleft = 1;
                int depthright = 1;
                for(HUDCUDLink l2:m_template.links) {
                    if (l!=l2 && l2.m_start>=l.m_start && l2.m_end<=l.m_end) depth++;
                    if (l!=l2 && l2.m_start==l.m_start && l2.m_end<=l.m_end) depthleft++;
                    if (l!=l2 && l2.m_start>=l.m_start && l2.m_end==l.m_end) depthright++;
                }

                Rectangle2D r2 = new Rectangle2D.Double(letterx[l.m_start] - depthleft*2, getHeight()/2 - h/2 - 4 - depth*2,
                                                        (letterxend[l.m_end-1] - letterx[l.m_start]) + depthleft*2 + depthright*2, h + 8 + depth*4);
                linkPositions.put(l,r2);
            }

            if (mouseOver!=null && selected!=null) {
                Rectangle2D rmo = linkPositions.get(mouseOver);
                Rectangle2D rs = linkPositions.get(selected);
                if (rmo!=null && rs!=null) {
                    if (rmo.contains(rs) && mouseOver!=selected) {
                        g.setColor(Color.RED);
                        g.fillRect((int)rmo.getX(),(int)rmo.getY(),(int)rmo.getWidth(),(int)rmo.getHeight());
                        g.setColor(Color.CYAN);
                        g.fillRect((int)rs.getX(),(int)rs.getY(),(int)rs.getWidth(),(int)rs.getHeight());
                    } else {
                        g.setColor(Color.CYAN);
                        g.fillRect((int)rs.getX(),(int)rs.getY(),(int)rs.getWidth(),(int)rs.getHeight());
                        g.setColor(Color.RED);
                        g.fillRect((int)rmo.getX(),(int)rmo.getY(),(int)rmo.getWidth(),(int)rmo.getHeight());
                    }
                }
            } else {
                if (mouseOver!=null) {
                    Rectangle2D r2 = linkPositions.get(mouseOver);
                    if (r2!=null) {
                        g.setColor(Color.RED);
                        g.fillRect((int)r2.getX(),(int)r2.getY(),(int)r2.getWidth(),(int)r2.getHeight());
                    }
                }
                if (selected!=null) {
                    Rectangle2D r2 = linkPositions.get(selected);
                    if (r2!=null) {
                        g.setColor(Color.CYAN);
                        g.fillRect((int)r2.getX(),(int)r2.getY(),(int)r2.getWidth(),(int)r2.getHeight());
                    }
                }
            }
            for(HUDCUDLink l:m_template.links) {
                Rectangle2D r2 = linkPositions.get(l);
                g.setColor(Color.BLACK);
                g.drawRect((int)r2.getX(),(int)r2.getY(),(int)r2.getWidth(),(int)r2.getHeight());
            }

            // draw the characters:
            for(int i = 0;i<length;i++) {
                String letter = m_template.text.charAt(i)+"";
                if (i>=selected_start && i<selected_end) {
                    g.setColor(Color.LIGHT_GRAY);
                    g.fillRect(letterx[i],getHeight()/2 - h/2, letterxend[i] - letterx[i], h);
                }
                g.setColor(Color.BLACK);
                g.drawString(letter,letterx[i],getHeight()/2 + h/2);
            }

        } else {
            linkPositions.clear();
            Rectangle2D r = g.getFontMetrics().getStringBounds("Select a template", g);
            g.setColor(Color.BLACK);
            g.drawString("Select a template",(int)(getWidth()-r.getWidth())/2,(int)(getHeight()-r.getHeight())/2);
        }
    }


    public void mouseClicked(int x,int y) {
        HUDCUDLink oldSelected = selected;
        selected = getLinkUnderMouse(x, y);
        if (oldSelected != selected) {
            m_parent.updateEntityAndExpressionsLists();
            m_parent.repaint();
        }
        
        selected_start = -1;
        selected_end = -1;
        createButtons();
    }
    
    
    public void mouseDoubleClicked(int x,int y) {
        // Find out where was the mouse clicked in the sentence:
        if (m_template!=null) {
            int character = -1;
            
            for(int i = 0;i<letterx.length;i++) {
                if (x>=letterx[i] && x<letterxend[i]) {
                    character = i;
                    break;
                }
            }
            
            if (character!=-1) {
                String text = m_template.text;
                // System.out.println("Double click at: " + m_template.text.charAt(character));
                // Find the boundaries of the word:
                int start = character;
                while(start>0 &&
                      ((text.charAt(start-1)>='a' &&
                        text.charAt(start-1)<='z') ||
                       (text.charAt(start-1)>='A' &&
                        text.charAt(start-1)<='Z'))) {
                    start--;
                }
                int end = character;
                while(end<text.length() &&
                      ((text.charAt(end)>='a' &&
                        text.charAt(end)<='z') ||
                       (text.charAt(end)>='A' &&
                        text.charAt(end)<='Z'))) {
                    end++;
                }
                String word = text.substring(start, end);
//                System.out.println("Selected word: '" + word + "'");
                
                // check that the entity doesn't already exist:
                HUDCUDLink undermouse = getLinkUnderMouse(x, y);
                if (undermouse!=null && undermouse.m_start==start && undermouse.m_end==end) {
                    if (undermouse != selected) {
                        m_parent.updateEntityAndExpressionsLists();
                        m_parent.repaint();
                        selected = undermouse;
                    }

                    selected_start = -1;
                    selected_end = -1;
                    createButtons();
                } else {             
                    
                    Object []possibilities = new Object[m_story.entities.size()+1];
                    possibilities[0] = "[new entity]";
                    int i = 1;
                    for(Entity e:m_story.entities) {
                        possibilities[i] = e.toString();
                        i++;
                    }
                    JFrame frame = new JFrame("Entity Creation Dialogue");
                    String s = (String)JOptionPane.showInputDialog(
                                        frame,
                                        "Create a new entity of link to an existing one?\n",
                                        "Entity Creation Dialogue",
                                        JOptionPane.PLAIN_MESSAGE,
                                        null,
                                        possibilities,
                                        "[new entity]");
                    if (s!=null && s.length()>0) {
                        if (s.equals("[new entity]")) {
                            HUDCUDLink l = m_template.newLink(start, end, new Entity("",null));
                            if (l!=null) {
                                m_story.entities.add((Entity)l.m_object);
                                selected = l;
                                createButtons();
                                m_parent.updateEntityAndExpressionsLists();
                            }
                        } else {
                            for(Entity e:m_story.entities) {
                                if (e.toString().equals(s)) {
                                    HUDCUDLink l = m_template.newLink(start, end, e);
                                    if (l!=null) {
                                        selected = l;
                                        createButtons();
                                        m_parent.updateEntityAndExpressionsLists();
                                        break;
                                    }
                                }
                            }
                        }
                    }                    
                    
                }                
            }
            
            repaint();
        }
    }
    
    
    public void setSelected(CUDObject e) {
        if (m_template==null) return;
        if (selected!=null && selected.m_object == e) return;
        for(HUDCUDLink l:m_template.links) {
            if (l.m_object==e) {
                selected = l;
                m_parent.updateEntityAndExpressionsLists();
                m_parent.repaint();
                selected_start = -1;
                selected_end = -1;
                createButtons();
                return;
            }
        }
    }


    public void mouseDragged(int x,int y) {
        // find the range of letters highlighted:
        if (m_template!=null) {
            int min_x = Math.min(x,original_mouse_drag_x);
            int max_x = Math.max(x,original_mouse_drag_x);
            int start = -1;
            int end = -1;
            
            for(int i = 0;i<letterx.length;i++) {
                if (min_x>=letterx[i] && min_x<letterxend[i]) {
                    start = i;
                    break;
                }
            }
            if (start==-1 && min_x<letterx[0]) start = 0;
            
            if (start!=-1) {
                for(int i = start+1;i<letterx.length;i++) {
                    if (max_x>=letterx[i] && max_x<letterxend[i]) {
                        end = i+1;
                        break;
                    }
                }
                if (end==-1 && max_x>letterxend[letterxend.length-1]) end = letterxend.length;
            }
            
            if (start!=-1 && end!=-1) {
                // selected text!!!
                selected_start = start;
                selected_end = end;
            } else {
                selected_start = -1;
                selected_end = -1;
            }
            repaint();
        }
    }


    public void mouseMoved(int x,int y) {
        HUDCUDLink oldMouseOver = mouseOver;
        mouseOver = getLinkUnderMouse(x, y);
        if (oldMouseOver != mouseOver) m_parent.repaint();
        
        original_mouse_drag_x = x;
        original_mouse_drag_y = y;
    }


    public HUDCUDLink getLinkUnderMouse(int x, int y) {
        HUDCUDLink over = null;
        for(HUDCUDLink l:linkPositions.keySet()) {
            Rectangle2D r = linkPositions.get(l);
            if (r.contains(x, y)) {
                if (over==null) {
                    over = l;
                } else {
                    Rectangle2D r2 = linkPositions.get(over);
                    if (r2.contains(r)) {
                        over = l;
                    }
                }
            }
        }
        return over;
    }
    

    public void actionPerformed(ActionEvent e) {
        String action;
        action = e.getActionCommand();
        if (action.equals("Delete")) {
            linkPositions.remove(selected);
            m_story.removeLink(selected.m_template, selected);
//            selected.m_template.removeLink(selected);
            selected = null;
            createButtons();
            m_parent.updateEntityAndExpressionsLists();
            m_parent.repaint();
        } else if (action.equals("comboBoxChanged")) {
            JComboBox cb = (JComboBox)e.getSource();
            if (selected.m_object instanceof Entity) {
                // Entity:
                Entity ent = (Entity)selected.m_object;
                ent.m_parent = entitiesListInComboBox.get(cb.getSelectedIndex());
                m_parent.updateEntityAndExpressionsLists();
                m_parent.repaint();
            } else {
                // Expression:
                Expression exp = (Expression)selected.m_object;
                exp.m_head = Expression.predefinedExpressionsNames[cb.getSelectedIndex()];
                m_parent.updateEntityAndExpressionsLists();
                m_parent.repaint();
            }
        }
    }

    public void keyTyped(KeyEvent ke) {
    }

    public void keyPressed(KeyEvent ke) {
    }

    public void keyReleased(KeyEvent ke) {
        JTextField tf = (JTextField)ke.getComponent();
        selected.m_object.m_id = tf.getText();
        m_parent.updateEntityAndExpressionsLists();
        m_parent.repaint();
    }
}
