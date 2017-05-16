/* Copyright 2010 Santiago Ontanon and Ashwin Ram */
package gui;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

public class TemplatePanelMouseListener implements MouseMotionListener, MouseListener {

    TemplatePanel m_editor;

    TemplatePanelMouseListener(TemplatePanel e) {
        m_editor = e;
    }

    public void mouseDragged(MouseEvent e) {
        m_editor.mouseDragged(e.getX(),e.getY());
    }

    public void mouseMoved(MouseEvent e) {
        m_editor.mouseMoved(e.getX(),e.getY());
    }

    public void mouseClicked(MouseEvent e) {
        if (e.getClickCount()==1) {
            m_editor.mouseClicked(e.getX(),e.getY());
        } else if (e.getClickCount()==2) {
            m_editor.mouseDoubleClicked(e.getX(),e.getY());
        }
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }
}