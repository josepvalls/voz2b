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
public class EntitySelectionListener implements ListSelectionListener {
    TemplatePanel templatePanel = null;
    
    public EntitySelectionListener(TemplatePanel tp) {
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
            // deselect any selected entities:
//            System.out.println("EntitySelectionListener: No entities selected!");
        } else {
            // Find out which indexes are selected.
            int minIndex = lsm.getMinSelectionIndex();
            int maxIndex = lsm.getMaxSelectionIndex();
            for (int i = minIndex; i <= maxIndex; i++) {
                if (lsm.isSelectedIndex(i)) {
//                    System.out.println("EntitySelectionListener: entity " + i + " selected!");
                    templatePanel.setSelected(templatePanel.getStory().entities.get(i));
                }
            }
        }
    }
}
