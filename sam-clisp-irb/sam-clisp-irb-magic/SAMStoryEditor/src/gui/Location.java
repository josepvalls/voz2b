/* Copyright 2010 Santiago Ontanon and Ashwin Ram */
package gui;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;


import java.util.LinkedList;
import java.util.List;
import story.Template;

class Location {
    public static final int RECTANGLE = 1;
    public static final int CIRCLE = 2;

    public int m_shape = RECTANGLE;
    public int m_dx = 32, m_dy = 32;
    public int m_radius = 16;
    public int m_x = 0, m_y = 0;
    public Template m_element = null;
    public TemplateSequencePanel m_editor = null;

    public Location(int x, int y, Template element, TemplateSequencePanel e) {
        m_x = x;
        m_y = y;
        m_dx = 32; 
        m_dy = 32;
        m_radius = 16;
        m_element = element;
        m_shape = CIRCLE;
        m_editor = e;
    }
    
    
    public Template getTemplate() {
        return m_element;
    }
    

    public boolean screen_point_inside(int x, int y) {
        return point_inside(m_editor.fromScreenX(x), m_editor.fromScreenY(y));
    }

    
    public boolean point_inside(double x, double y) {
        if (m_shape == CIRCLE) {
            double sqd = (m_x - x) * (m_x - x) + (m_y - y) * (m_y - y);
            if (sqd < m_radius * m_radius) {
                return true;
            }
        }
        if (m_shape == RECTANGLE) {            
            if (x > m_x - (m_dx / 2)
                && x < m_x + (m_dx / 2)
                && y > m_y - (m_dy / 2)
                && y < m_y + (m_dy / 2)) {
//                System.out.println(x + "," + y + " in " + m_x + "," + m_y);
                return true;
            }
        }

        return false;
    }
    

    public void draw(Graphics g, Color highlight) {
        if (highlight==null) highlight = Color.lightGray;
        if (m_shape == CIRCLE) {
            g.setColor(highlight);
            g.fillArc(m_editor.toScreenX(m_x - m_radius),
                m_editor.toScreenY(m_y - m_radius),
                m_editor.toScreen(m_radius * 2),
                m_editor.toScreen(m_radius * 2), 0, 360);
            g.setColor(Color.BLACK);
            g.drawArc(m_editor.toScreenX(m_x - m_radius),
                m_editor.toScreenY(m_y - m_radius),
                m_editor.toScreen(m_radius * 2),
                m_editor.toScreen(m_radius * 2), 0, 360);
        } 
        
        if (m_shape == RECTANGLE) {
            g.setColor(highlight);
            g.fillRect(m_editor.toScreenX(m_x - (m_dx / 2)), m_editor.toScreenY(m_y - (m_dy / 2)),
                m_editor.toScreen(m_dx), m_editor.toScreen(m_dy));
            g.setColor(Color.BLACK);
            g.drawRect(m_editor.toScreenX(m_x - (m_dx / 2)), m_editor.toScreenY(m_y - (m_dy / 2)),
                m_editor.toScreen(m_dx), m_editor.toScreen(m_dy));
        }

        g.setColor(Color.BLACK);
        String name = m_element.ID;
        Rectangle2D b = g.getFontMetrics().getStringBounds(name, g);
        g.drawString(name,
                        (int) (m_editor.toScreenX(m_x) - b.getWidth() / 2),
                        (int) (m_editor.toScreenY(m_y) + b.getHeight() / 2));
        
    }
    
    
    public void drawLink(Location e2, Graphics g) {

        {
            double vx = m_x - e2.m_x;
            double vy = m_y - e2.m_y;
            double n = Math.sqrt(vx * vx + vy * vy);
            if (n != 0) {
                vx /= n;
                vy /= n;
                double cvx = -vy;
                double cvy = vx;
                double start_x = e2.m_x;
                double start_y = e2.m_y;
                double f = 0.0;
                boolean found = false;
                while (!found && f < n) {
                    f += 1.0;
                    if (!e2.point_inside((int) (start_x + f * vx), (int) (start_y + f * vy))) {
                        found = true;
                    }
                }

                g.setColor(Color.black);
                if (found) {/* Copyright 2010 Santiago Ontanon and Ashwin Ram */


                    start_x += f * vx;
                    start_y += f * vy;
                    int px[] = {m_editor.toScreenX(start_x), m_editor.toScreenX(start_x + vx * 10 + cvx * 5), m_editor.toScreenX(start_x + vx * 10 - cvx * 5)};
                    int py[] = {m_editor.toScreenY(start_y), m_editor.toScreenY(start_y + vy * 10 + cvy * 5), m_editor.toScreenY(start_y + vy * 10 - cvy * 5)};

                    g.fillPolygon(px, py, 3);
                    g.drawLine(m_editor.toScreenX(m_x), m_editor.toScreenY(m_y),
                        m_editor.toScreenX(start_x), m_editor.toScreenY(start_y));
                } else {
                    g.drawLine(m_editor.toScreenX(m_x), m_editor.toScreenY(m_y),
                        m_editor.toScreenX(e2.m_x), m_editor.toScreenY(e2.m_y));
                } // if (found)
            }
        }
    }    




    public Rectangle boundingBox() {
        Rectangle r = new Rectangle(m_x, m_y, 0, 0);
        if (m_shape == CIRCLE) {
            r.setFrame(m_x - m_radius, m_y - m_radius, m_radius * 2, m_radius * 2);
        } else if (m_shape == RECTANGLE) {
            r.setFrame(m_x - m_dx / 2, m_y - m_dy / 2, m_dx, m_dy);
        }
        return r;
    }
}
