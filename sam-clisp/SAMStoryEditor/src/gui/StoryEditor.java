/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import story.Story;

/**
 *
 * @author santi
 */
public class StoryEditor extends JFrame {
    Story story = null;
    
    public static void main(String []args) throws Exception {
        Story s = new Story();
        StoryEditor se = new StoryEditor(s);
        se.setDefaultCloseOperation(EXIT_ON_CLOSE);
        se.setVisible(true);
    }
    
    public StoryEditor(Story a_story) {
        super("SAM Story Editor v1.11");
        
        story = a_story;
        
        setSize(1024,768); 
 
//        JTabbedPane jtp = new JTabbedPane();
//        getContentPane().add(jtp);
        
//        JPanel templateCreator = new SimpleTemplateCreator(story);//This will create the first tab
        JPanel templateCreator = new StoryEditorPanel(story);//This will create the first tab
//        JPanel templateEditor = new JPanel();//This will create the second tab
 
//        jtp.addTab("Template Creator", templateCreator);
//        jtp.addTab("Template Editor", templateEditor);
        getContentPane().add(templateCreator);
    }
    
}
