/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import story.Story;

/**
 *
 * @author santi
 */
public class ExpressionSelectionListener implements ListSelectionListener {
    TemplatePanel templatePanel = null;
    
    public ExpressionSelectionListener(TemplatePanel tp) {
        super();
        templatePanel = tp;
    }
    
    public void valueChanged(ListSelectionEvent e) {
//        System.out.println(e);
        ListSelectionModel lsm = (ListSelectionModel)e.getSource();

        int firstIndex = e.getFirstIndex();
        int lastIndex = e.getLastIndex();
        boolean isAdjusting = e.getValueIsAdjusting();

        if (lsm.isSelectionEmpty()) {
            // deselect any selected expressions:
//            System.out.println("ExpressionSelectionListener: No expressions selected!");
        } else {
            // Find out which indexes are selected.
            int minIndex = lsm.getMinSelectionIndex();
            int maxIndex = lsm.getMaxSelectionIndex();
            for (int i = minIndex; i <= maxIndex; i++) {
                if (lsm.isSelectedIndex(i)) {
//                    System.out.println("ExpressionSelectionListener: expression " + i + " selected!");
                    templatePanel.setSelected(templatePanel.getStory().expressions.get(i));
                }
            }
        }
    }
}
