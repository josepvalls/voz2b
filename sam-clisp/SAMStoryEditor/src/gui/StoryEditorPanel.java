/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.StringTokenizer;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;
import story.*;

/**
 *
 * @author santi
 */
public class StoryEditorPanel extends JPanel implements ActionListener, DocumentListener {

    // Template sequencer creatino:
    static final String LOAD = "Load";
    static final String SAVE = "Save";
    static final String ADD = "Add";
    static final String REMOVE_TEMPLATE = "Remove";
    static final String SEQUENCE = "Sequence";
    static final String REVERSE = "Reverse";
    static final String REFRESH = "<html>Refresh<br>Keywords</html>";
    static String top_buttons[] = {LOAD,SAVE,ADD, REMOVE_TEMPLATE, SEQUENCE, REVERSE, REFRESH};
    
    // Template buttons:
    static final String ENTITY = "New Entity";
    static final String EXISTING_ENTITY = "Existing Entity";
    static final String EXPRESSION = "New Expression";
    static String bottom_buttons[] = {ENTITY, EXISTING_ENTITY, EXPRESSION};
    
    Story story = null;
    JTextArea freeTextArea = null;
    TemplateSequencePanel seqPanel = null;
    TemplatePanel templatePanel = null;
    CUDPanel CUDpanel = null;
    JList entityList = null;
    JList expressionList = null;
    
    public StoryEditorPanel(Story a_story) {
        story = a_story;
        
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        {
            JPanel topPanel = new JPanel();
            topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.X_AXIS));
            {
                freeTextArea = new JTextArea(15, 20);
                freeTextArea.setLineWrap(true);
                freeTextArea.setWrapStyleWord(false);
                freeTextArea.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
                freeTextArea.setFont(new Font("monospaced", Font.PLAIN, 12));
                freeTextArea.setSize(512, 512);
                freeTextArea.getDocument().addDocumentListener(this);
                JScrollPane scrollingText = new JScrollPane(freeTextArea);                
                topPanel.add(scrollingText);
            }
            
            {
                JPanel tmp = new JPanel();
                tmp.setLayout(new BoxLayout(tmp, BoxLayout.Y_AXIS));
                tmp.setMaximumSize(new Dimension(100,300));
                for (String button : top_buttons) {
                    JButton b = new JButton(button);
                    b.setActionCommand(button);
                    b.addActionListener(this);
                    b.setAlignmentX(Component.CENTER_ALIGNMENT);
//                    b.setMinimumSize(new Dimension(64,-1));
//                    b.setPreferredSize(new Dimension(96,-1));
//                    b.setMaximumSize(new Dimension(96,-1));
//                    b.getMaximumSize().width = 96;
                    tmp.add(b);
                    
                    if (button.equals(SAVE) || button.equals(REVERSE)) {
                        tmp.add(Box.createVerticalStrut(16));
                    }
                }
                topPanel.add(tmp);
            }
            
            {
                seqPanel = new TemplateSequencePanel(a_story, this);
                seqPanel.setSize(512, 512);
                topPanel.add(seqPanel);
            }
            add(topPanel);
        }
        
        {
            JPanel bottomPanel = new JPanel();
            bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.X_AXIS));
            {
                JPanel bottomLeftPanel = new JPanel();
                bottomLeftPanel.setLayout(new BoxLayout(bottomLeftPanel, BoxLayout.Y_AXIS));

                {
                    JPanel tmp = new JPanel();
                    tmp.setLayout(new BoxLayout(tmp, BoxLayout.X_AXIS));
                    for (String button : bottom_buttons) {
                        JButton b = new JButton(button);
                        b.setActionCommand(button);
                        b.addActionListener(this);
//                        b.setAlignmentX(Component.CENTER_ALIGNMENT);
                        tmp.add(b);
                    }
                    bottomLeftPanel.add(tmp);
                }
                templatePanel = new TemplatePanel(a_story, seqPanel, this);
                templatePanel.setSize(512, 512);
                bottomLeftPanel.add(new JScrollPane(templatePanel));
                
                bottomPanel.add(bottomLeftPanel);
            }
            entityList = new JList(new DefaultListModel());
            entityList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            entityList.setSize(150, 256);
            entityList.getSelectionModel().addListSelectionListener(new EntitySelectionListener(templatePanel));
            expressionList = new JList(new DefaultListModel());
            expressionList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            expressionList.setSize(150, 256);
            expressionList.getSelectionModel().addListSelectionListener(new ExpressionSelectionListener(templatePanel));
            bottomPanel.add(new JScrollPane(entityList));
            bottomPanel.add(new JScrollPane(expressionList));
            
            CUDpanel = new CUDPanel();
            bottomPanel.add(CUDpanel);
            
            add(bottomPanel);
        }
        
        
        // add some test text, and create some tempaltes form it:
/*
        freeTextArea.setText("Ales used to have a bird when he was young.\n"
                + "Ales was very fond of it.\n"
                + "But the bird died, leaving ALES really sad.\n"
                );        
        automaticallyGenerateTemplates(freeTextArea.getText());
        */
        
        try {
//            Story s = Story.load(new File("/Users/santi/NetBeansProjects/SAMStoryEditor/julian-memory2.txt"));
//            if (s!=null) reset(s);
        } catch(Exception e) {
            e.printStackTrace();
        }
        
    }
    
    public void refreshHighlights() {
        Highlighter hl = freeTextArea.getHighlighter();
        hl.removeAllHighlights();
        String text = null;
        try {
            text = freeTextArea.getDocument().getText(0, freeTextArea.getDocument().getLength());
            
            for (Template t : story.getTemplates()) {
                int index = text.indexOf(t.text);
                if (index != -1) {
                    // add highlighter:
                    Color c = Color.LIGHT_GRAY;
                    if (seqPanel.getSelected().contains(t)) {
                        c = Color.CYAN;
                    }
                    if (seqPanel.mouseOver == t) {
                        c = Color.RED;
                    }
                    DefaultHighlighter.DefaultHighlightPainter hl1 = new DefaultHighlighter.DefaultHighlightPainter(c);
                    hl.addHighlight(index, index + t.text.length(), hl1);
                }
            }
        } catch (BadLocationException ex) {
            ex.printStackTrace();
        }
    }
    
    public void actionPerformed(ActionEvent e) {
        String Action;
        Action = e.getActionCommand();
        if (Action.equals(SAVE)) {
            JFileChooser chooser = new JFileChooser();
            int returnVal = chooser.showSaveDialog(this);
            if(returnVal == JFileChooser.APPROVE_OPTION) {
                story.save(chooser.getSelectedFile());
            }            
        } else if (Action.equals(LOAD)) {
            JFileChooser chooser = new JFileChooser();
            int returnVal = chooser.showOpenDialog(this);
            if(returnVal == JFileChooser.APPROVE_OPTION) {
                Story s = Story.load(chooser.getSelectedFile());
                if (s!=null) reset(s);
            }
        } else if (Action.equals(ADD)) {
            newTemplate(freeTextArea.getSelectedText());
        } else if (Action.equals(REMOVE_TEMPLATE)) {
            for (Template t : seqPanel.getSelected()) {
                story.removeTemplate(t);
            }
            refreshHighlights();            
        } else if (Action.equals(SEQUENCE)) {
            List<Template> l = seqPanel.getSelected();
            if (l.size() == 2) {
                if (story.sequenceP(l.get(0), l.get(1)) || 
                    story.sequenceP(l.get(1), l.get(0))) {
                    story.removeSequence(l.get(0), l.get(1));
                    story.removeSequence(l.get(1), l.get(0));
                } else {
                    story.sequence(l.get(0), l.get(1));
                }
            }
        } else if (Action.equals(REVERSE)) {
            List<Template> l = seqPanel.getSelected();
            if (l.size() == 2) {
                if (story.sequenceP(l.get(0), l.get(1))) {
                    story.removeSequence(l.get(0), l.get(1));
                    story.sequence(l.get(1), l.get(0));                    
                } else {
                    story.removeSequence(l.get(1), l.get(0));
                    story.sequence(l.get(0), l.get(1));                    
                }                
            }
        } else if (Action.equals(ENTITY)) {
            if (templatePanel.m_template!=null) {
                HUDCUDLink l = templatePanel.m_template.newLink(templatePanel.selected_start, templatePanel.selected_end, new Entity("",null));
                if (l!=null) {
                    story.entities.add((Entity)l.m_object);
                    templatePanel.selected = l;
                    templatePanel.createButtons();
                    updateEntityAndExpressionsLists();
                }
            }
        } else if (Action.equals(EXISTING_ENTITY)) {
            // Add the entity that is selected in the JList:
            int selected = entityList.getSelectedIndex();
            if (selected>=0) {
                Entity entity = story.entities.get(selected);
                HUDCUDLink l = templatePanel.m_template.newLink(templatePanel.selected_start, templatePanel.selected_end, entity);
                if (l!=null) {
                    templatePanel.selected = l;
                    templatePanel.createButtons();
                    updateEntityAndExpressionsLists();
                }                
            }
        } else if (Action.equals(EXPRESSION)) {
            if (templatePanel.m_template!=null) {
                HUDCUDLink l = templatePanel.m_template.newLink(templatePanel.selected_start, templatePanel.selected_end, new Expression(""));
                if (l!=null) {
                    story.expressions.add((Expression)l.m_object);
                    templatePanel.selected = l;
                    templatePanel.createButtons();
                    updateEntityAndExpressionsLists();
                }
            }
        } else if (Action.equals(REFRESH)) {
            Entity.reloadFromDisk();
            Expression.reloadFromDisk();
        }
        seqPanel.updateLocations();
        seqPanel.repaint();
    }
    
    
    public void reset(Story s) {
        story = s;
        
        {
            String text = "";
            for(Template t:story.getTemplates()) {
                text += t.text + "\n";
            }
            freeTextArea.setText(text);
        }
        
        seqPanel.reset(story);
        templatePanel.reset(story);
        updateEntityAndExpressionsLists();
        refreshHighlights();
        repaint();        
    }
    
    public void newTemplate(String text) {
        boolean repeated = false;
        int i = 1;
        String name = "t1";
        while (true) {
            name = "t" + i;
            if (story.getTemplate(name) == null) {
                break;
            }
            i++;
        }            
        if (text != null && !text.equals("")) {
            for (Template t : story.getTemplates()) {
                if (t.text.equals(text)) {
                    repeated = true;
                    break;
                }
            }
            if (!repeated) {
                Template t = new Template(name, text);
                story.templates.add(t);
            }
        }
        refreshHighlights();
    }
    
    
    public void updateEntityAndExpressionsLists() {
        DefaultListModel model = (DefaultListModel)entityList.getModel();
        model.clear();
        
        int i = 0;
        int selected = -1;
        for(Entity e:story.entities) {
            model.add(i,e);
            if (templatePanel.selected!=null && e.equals(templatePanel.selected.m_object)) selected = i;
            i++;
        }
        if (selected==-1) entityList.clearSelection();
                     else entityList.setSelectedIndex(selected);
    
        model = (DefaultListModel)expressionList.getModel();
        model.clear();
        
        i=0;
        selected = -1;
        for(Expression e:story.expressions) {
            model.add(i,e);
            if (templatePanel.selected!=null && templatePanel.selected.m_object == e) selected = i;
            i++;
        }
        if (selected==-1) expressionList.clearSelection();
                     else expressionList.setSelectedIndex(selected);
    }
    
    
    public void automaticallyGenerateTemplates(String text) {
        StringTokenizer st = new StringTokenizer(text,".\n\r");
        
        while(st.hasMoreTokens()) {
            String token = st.nextToken();
            token = token.trim();
            if (token.length()>0) newTemplate(token);
        }
        seqPanel.updateLocations();
    }
    
    public void insertUpdate(DocumentEvent de) {
        refreshHighlights();
    }
    
    public void removeUpdate(DocumentEvent de) {
        refreshHighlights();
    }
    
    public void changedUpdate(DocumentEvent de) {
        refreshHighlights();
    }
}
