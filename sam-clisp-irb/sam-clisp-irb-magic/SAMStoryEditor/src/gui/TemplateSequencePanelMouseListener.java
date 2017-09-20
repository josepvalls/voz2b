/* Copyright 2010 Santiago Ontanon and Ashwin Ram */
package gui;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

public class TemplateSequencePanelMouseListener implements MouseMotionListener, MouseListener {

    TemplateSequencePanel m_editor;
    int last_x = -1, last_y = -1;
    int dragged_offset_x = 0, dragged_offset_y = 0;
    Location m_dragging = null;

    TemplateSequencePanelMouseListener(TemplateSequencePanel e) {
        m_editor = e;
    }

    public void mouseDragged(MouseEvent e) {
        m_editor.mouseDragged(e.getX(),e.getY());
//        System.out.println(e.getX() + "," + e.getY());
    }

    public void mouseMoved(MouseEvent e) {
        m_editor.mouseMoved(e.getX(),e.getY());
    }

    public void mouseClicked(MouseEvent e) {
        m_editor.mouseClicked(e.getX(),e.getY(),e.getButton());
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
        last_x = -1;
        last_y = -1;
        m_dragging = null;
        m_editor.recomputePhases();
        m_editor.parent.repaint();
    }
}