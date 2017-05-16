/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package story;

import java.io.*;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import lispParser.LispNode;

/**
 *
 * @author santi
 */
public class Story {
    public List<Template> templates = new LinkedList<Template>();
    public HashMap<Template,List<Template>> templateSequence = new HashMap<Template,List<Template>>();
    
    public List<Entity> entities = new LinkedList<Entity>();
    public List<Expression> expressions = new LinkedList<Expression>(); 
    
    public Template getTemplate(String name) {
        for(Template t: templates) {
            if (t.ID.equals(name)) return t;
        }
        return null;
    }
    
    
    public Entity getEntity(String name) {
        for(Entity e:entities) if (e.m_id.equals(name)) return e;
        return null;
    }
    
    public List<Entity> getAllEntities() {
        List<Entity> l = new LinkedList<Entity>();
        for(Entity e:Entity.predefinedEntities) l.add(e);
        for(Entity e:entities) l.add(e);
        
        return l;
    }
    
    public Expression getExpression(String name) {
        for(Expression e:expressions) if (e.m_id.equals(name)) return e;
        return null;
    }
    
    public CUDObject getCUDObject(String name) {
        for(Entity e:entities) if (e.m_id.equals(name)) return e;
        for(Expression e:expressions) if (e.m_id.equals(name)) return e;
        return null;
    }

    public void removeTemplate(int index) {
        Template t = templates.get(index);
        if (t!=null) removeTemplate(t);
    }
    
    
    public void removeTemplate(Template t) {
        templates.remove(t);
        templateSequence.remove(t);
        for(List<Template> l:templateSequence.values()) l.remove(t);
    }
    
    
    public void removeLink(Template t,HUDCUDLink l) {
        t.removeLink(l);   
        for(Template t2:templates) {
            if (t2.containsLinkTo(l.m_object)) return;
        }
        
        if (l.m_object instanceof Entity) {
            entities.remove((Entity)l.m_object);
        } else {
            expressions.remove((Expression)l.m_object);
        }
    }
    
    public boolean sequenceP(Template t1, Template t2) {
        List<Template> l = templateSequence.get(t1);
        if (l!=null) 
            return l.contains(t2);
        return false;
    }    
    
    
    public void sequence(Template t1, Template t2) {
        List<Template> l = templateSequence.get(t1);
        if (l==null) {
            l = new LinkedList<Template>();
            templateSequence.put(t1,l);
        }
        l.add(t2);
    }
    
    
    public void removeSequence(Template t1, Template t2) {
        List<Template> l = templateSequence.get(t1);
        if (l!=null) l.remove(t2);
    }
    
    
    public List<Template> getTemplates() {
        return templates;
    }
    
    public void save(File f) {
        try {
            FileWriter fw = new FileWriter(f);
            
            // cluster templates by phases:
            int minphase = templates.get(0).phase;
            int maxphase = templates.get(0).phase;
            HashMap<Integer,List<Template>> phases = new HashMap<Integer,List<Template>>();
            for(Template t:templates) {
                List<Template> l = phases.get(t.phase);
                if (l==null) {
                    l = new LinkedList<Template>();
                    phases.put(t.phase,l);
                }
                l.add(t);
                if (minphase>t.phase) minphase = t.phase;
                if (maxphase<t.phase) maxphase = t.phase;
            }
            List<CUDObject> common = new LinkedList<CUDObject>();
            
            // Compute the CUDObject that are common to more than one phase:
            for(Entity e:entities) {
                int inPhase = -1;
                for(Integer phase:phases.keySet()) {
                    List<Template> l = phases.get(phase);
                    for(Template t:l) {
                        if (t.containsLinkTo(e)) {
                            if (inPhase!=-1 && phase!=inPhase) {
                                common.add(e);
                                break;
                            }
                            inPhase = phase;
                        }
                    }
                    if (common.contains(e)) break;
                }
            }
            for(Expression e:expressions) {
                int inPhase = -1;
                for(Integer phase:phases.keySet()) {
                    List<Template> l = phases.get(phase);
                    for(Template t:l) {
                        if (t.containsLinkTo(e)) {
                            if (inPhase!=-1 && phase!=inPhase) {
                                common.add(e);
                                for(CUDObject o:e.recursiveChildren()) common.add(o);
                                break;
                            }
                            inPhase = phase;
                        }
                    }
                    if (common.contains(e)) break;
                }
            }
            
//            System.out.println("Common: " + common);
            
            fw.write(";; --------------------------------------------------------------------\n");
            fw.write(";; This file has been automatically generated by SAM Story Editor v1.11\n");
            fw.write(";; Santiago Ontanon\n");
            fw.write(";; Jichen Zhu\n");
            fw.write(";; --------------------------------------------------------------------\n");
            fw.write("\n");
            fw.write("(setf *story*\n");
            fw.write("  '(STORY-0\n");
            fw.write("     (:discourse\n");
            fw.write("       (:clauses\n");
            fw.write("         (c0 (:s");
            for(int phase = minphase;phase<=maxphase;phase++) {
                if (phases.get(phase)!=null) fw.write(" phase" + phase);
            }
            fw.write("))\n");
            for(int phase = minphase;phase<=maxphase;phase++) {
                if (phases.get(phase)!=null) {
                    for(String c:generateClausesForPhase(phase,phases.get(phase))) 
                        fw.write("         " + c + "\n");
                }
            }
            fw.write("       )\n");
            fw.write("       (:templates\n");
            for(Template t:templates) {
                fw.write("         ;; " + t.text + "\n");
            }
            for(Template t:templates) {
                fw.write("         " + t + "\n");
            }
            fw.write("       )\n");
            fw.write("     )\n");
            fw.write("     (:structure\n");
            fw.write("       (common\n");
            fw.write("         (:entities\n");
            for(CUDObject o:common) {
                if (o instanceof Entity) 
                    fw.write("           " + o + "\n");
            }
            fw.write("         )\n");            
            fw.write("         (:expressions\n");
            for(CUDObject o:common) {
                if (o instanceof Expression) 
                    fw.write("           " + o + "\n");
            }
            fw.write("         )\n");            
            fw.write("       )\n");
            for(int phase = minphase;phase<=maxphase;phase++) {
                List<Template> phaseTemplates = phases.get(phase);
                if (phaseTemplates!=null) {
                    fw.write("       (phase" + phase + "\n");
                    fw.write("         (:entities\n");
                    for(CUDObject o:entities) {
                        if (!common.contains(o)) {
                            boolean inPhase = false;
                            for(Template t:phaseTemplates) {
                                if (t.containsLinkTo(o)) inPhase = true;
                            }
                            if (inPhase) fw.write("           " + o + "\n");
                        }
                    }
                    fw.write("         )\n");            
                    fw.write("         (:expressions\n");
                    for(CUDObject o:expressions) {
                        if (!common.contains(o)) {
                            boolean inPhase = false;
                            for(Template t:phaseTemplates) {
                                if (t.containsLinkTo(o)) inPhase = true;
                            }
                            if (inPhase) fw.write("           " + o + "\n");
                        }
                    }
                    fw.write("         )\n");            
                    fw.write("       )\n");                    
                }
            }            
            fw.write("     )\n");
            fw.write("   )\n");
            fw.write(")");
            fw.flush();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
    
    
    List<String> generateClausesForPhase(int phase, List<Template> tl) {
        List<String> clauses = new LinkedList<String>();
        
        // cluster the templates into clickes:
        List<List<Template>> clickes = new LinkedList<List<Template>>();
        List<Template> startingPoints = new LinkedList<Template>();
        
        {
            List<Template> tl2 = new LinkedList<Template>();
            tl2.addAll(tl);
            while(!tl2.isEmpty()) {
                List<Template> clicke = reachableFrom(tl2.get(0));
                tl2.removeAll(clicke);
                clickes.add(clicke);
            }
        }
        
        // Select the start node of each clicke:
        for(List<Template> clicke:clickes) {
            Template start = null;
            List<Template> reachableFromStart = null;
            for(Template t:clicke) {
                if (start==null) {
                    start = t;
                    reachableFromStart = reachableFrom(t);
                } else {
                    List<Template> tmp = reachableFrom(t);
                    if (tmp.containsAll(reachableFromStart)) {
                        if (!reachableFromStart.containsAll(tmp)) {
                            start = t;
                            reachableFromStart = tmp;                            
                        } else if (templates.indexOf(start)>templates.indexOf(t)) {
                            start = t;
                            reachableFromStart = tmp;                            
                        }
                    }
                }
            }
            startingPoints.add(start);            
        }
        
        // generate the initial clause:
        {
            String c1 = "(phase" + phase + " (:a";
            for(Template t:startingPoints) c1 += " c_" + t.ID;
            c1 += "))";
            clauses.add(c1);
        }
        
        // generate a clause for each template:
        for(Template t:tl) {
            String c = "(c_" + t.ID + " (:s " + t.ID;
            List<Template> next = templateSequence.get(t);
            if (next==null) {
                c+="))";
            } else {
                if (next.size()==1) {
                    c+=" c_" + next.get(0).ID + "))";
                } else {
                    c+=" (:a ";
                    for(Template t2:next) c+=" c_" + t2.ID;
                    c+=")))";
                }
            }
            clauses.add(c);
        }
        
        return clauses;
    }
    
    List<Template> reachableFrom(Template t) {
        List<Template> reachable = new LinkedList<Template>();
        List<Template> open = new LinkedList<Template>();
        open.add(t);
        while(!open.isEmpty()) {
            t = open.remove(0);
            reachable.add(t);
            List<Template> next = templateSequence.get(t);
            if (next!=null) {
                for(Template t2:next) {
                    if (!reachable.contains(t2) && !open.contains(t2)) open.add(t2);
                }
            }
        }
        return reachable;
    }
    
    
    public static Story load(File f) {
        try {
            LispNode n = LispNode.parse(new BufferedReader(new FileReader(f)));
//            System.out.println("Lisp structure parsed...");
//            System.out.println(n);
            if (n==null) return null;
            
            LispNode discourse = n.searchSymbol(":discourse").parent;
            LispNode structure = n.searchSymbol(":structure").parent;
            
            if (discourse==null || structure==null) return null;
            
            Story s = new Story();
            if (s.loadStructure(structure) &&
                s.loadDiscourse(discourse)) return s;
            
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return null;
    }
    
    
    public boolean loadDiscourse(LispNode discourse) {
        LispNode clauses = discourse.searchSymbol(":clauses").parent;
        LispNode templates = discourse.searchSymbol(":templates").parent;
        
        if (clauses==null || templates==null) return false;
        
        // load templates:
        for(LispNode tn:templates.children) {
            if (tn.type==LispNode.TYPE_LIST) {
                // template!
                if (!loadTemplate(tn)) return false;
            }
        }
        
        // load clauses (assign phases to templates):
        int phase = 1;
        LispNode phaseClause = null;
        List<LispNode> clausesLoaded = new LinkedList<LispNode>();
        do {
            phaseClause = clauses.searchListStartingBySymbol("phase" + phase);
            if (phaseClause!=null) {
                clausesLoaded.add(phaseClause);
                if (!loadClause(phaseClause, clauses, phase, clausesLoaded)) return false;
            }
            phase++;
        }while(phaseClause!=null);
        
        return true;
    }
    
    
    public boolean loadClause(LispNode pc, LispNode clauses, int phase, List<LispNode> clausesLoaded) {
       // System.out.println("Loading clause " + pc.children.get(0).value);
        LispNode body = pc.children.get(1);
        return loadClauseBody(body,clauses,phase, clausesLoaded);
    }
    
    
    public boolean loadClauseBody(LispNode body, LispNode clauses, int phase, List<LispNode> clausesLoaded) {
        if (body.type==LispNode.TYPE_SYMBOL) {
            Template t = getTemplate(body.value);
            if (t!=null) {
                t.phase = phase;
                // System.out.println("Phase " + phase + " -> " + t.ID);
                return true;
            } else {
                LispNode clause = clauses.searchListStartingBySymbol(body.value);
                if (clause!=null) {
                    if (!clausesLoaded.contains(clause)) {
                        clausesLoaded.add(clause);
                        if (!loadClause(clause, clauses, phase, clausesLoaded)) return false;
                    }
                } else {
                    System.err.println("Story.loadClauseBody: unrecognized symbol " + body);
                    return false;
                } 
            }
            return true;
        } else if (body.type==LispNode.TYPE_LIST) {            
            String type = body.children.get(0).value;
            if (type.equals(":s")) {
                return loadClauseSequence(body, clauses, phase, clausesLoaded);
            } else if (type.equals(":a")) {
                return loadClauseAlternative(body, clauses, phase, clausesLoaded);
            } else if (type.equals(":r")) {
                return loadClauseRepetition(body, clauses, phase, clausesLoaded);
            } else {
                System.err.println("Story.loadClauseBody: Unrecognized clause command: " + type);
                return false;
            }
        } else {
                System.err.println("Story.loadClauseBody: cannod load clause body: " + body);
            return false;
        }
    }
 
    
    public boolean loadClauseSequence(LispNode body, LispNode clauses, int phase, List<LispNode> clausesLoaded) {    
        // First load the inner clauses:
        for(int i = 1;i<body.children.size();i++) {
            if (!loadClauseBody(body.children.get(i),clauses,phase,clausesLoaded)) return false;            
        }
        
        // Now set the restrictions of the sequence:
        for(int i = 1;i<body.children.size()-1;i++) {
            List<Template> last = lastOfClause(body.children.get(i), clauses);
            List<Template> first = firstOfClause(body.children.get(i+1), clauses);
            // set the order relations:
            for(Template t1:last) {
                for(Template t2:first) {
                    if (t1!=t2) sequence(t1,t2);
                }
            }
        }
        return true;
    }    
    
    
    public boolean loadClauseAlternative(LispNode body, LispNode clauses, int phase, List<LispNode> clausesLoaded) {    
        for(int i = 1;i<body.children.size();i++) {
            if (!loadClauseBody(body.children.get(i),clauses,phase,clausesLoaded)) return false;
        }
        return true;
    }    
 
    
    public boolean loadClauseRepetition(LispNode body, LispNode clauses, int phase, List<LispNode> clausesLoaded) {    
        for(int i = 1;i<body.children.size();i++) {
            if (!loadClauseBody(body.children.get(i),clauses,phase,clausesLoaded)) return false;
        }
        List<Template> first = lastOfClause(body.children.get(1), clauses);
        List<Template> last = firstOfClause(body.children.get(body.children.size()-1), clauses);
        // set the order relations:
        for(Template t1:last) {
            for(Template t2:first) {
                if (t1!=t2) sequence(t2,t1);
            }
        }
        return true;
    }    
    
    
    public List<Template> lastOfClause(LispNode body, LispNode clauses) {
        if (body.type==LispNode.TYPE_SYMBOL) {
            Template t = getTemplate(body.value);
            if (t!=null) {
                List<Template> l = new LinkedList<Template>();
                l.add(t);
                return l;
            } else {
                LispNode clause = clauses.searchListStartingBySymbol(body.value);
                if (clause!=null) {
                    return lastOfClause(clause.children.get(1),clauses);
                } else {
                    System.err.println("Story.lastOfClause: unrecognized symbol " + body);
                    return null;
                } 
            }
        } else if (body.type==LispNode.TYPE_LIST) {
            LispNode head = body.children.get(0);
            if (head.value.equals(":s")) {
                return lastOfClause(body.children.get(body.children.size()-1),clauses);
            } else if (head.value.equals(":a")) {
                List<Template> l = new LinkedList<Template>();
                for(int i = 1;i<body.children.size();i++) {
                    l.addAll(lastOfClause(body.children.get(i),clauses));
                }
                return l;
            } else if (head.value.equals(":r")) {
                // there is no last element in a repetition, so return the empty list:
                return new LinkedList<Template>();
            } else {
                System.err.println("Story.lastOfClause: unrecognized body head: " + head);
                return null;
            }
        } else {
            System.err.println("Story.lastOfClause: unrecognized body: " + body);
            return null;            
        }
    }
 
    
    public List<Template> firstOfClause(LispNode body, LispNode clauses) {    
        if (body.type==LispNode.TYPE_SYMBOL) {
            Template t = getTemplate(body.value);
            if (t!=null) {
                List<Template> l = new LinkedList<Template>();
                l.add(t);
                return l;
            } else {
                LispNode clause = clauses.searchListStartingBySymbol(body.value);
                if (clause!=null) {
//                    System.out.println("Story.firstOfClause: recursing on " + body.value);
                    return firstOfClause(clause.children.get(1),clauses);
                } else {
                    System.err.println("Story.firstOfClause: unrecognized symbol " + body);
                    return null;
                } 
            }
        } else if (body.type==LispNode.TYPE_LIST) {
            LispNode head = body.children.get(0);
            if (head.value.equals(":s")) {
                return firstOfClause(body.children.get(1),clauses);
            } else if (head.value.equals(":a")) {
                List<Template> l = new LinkedList<Template>();
                for(int i = 1;i<body.children.size();i++) {
                    l.addAll(firstOfClause(body.children.get(i),clauses));
                }
                return l;
            } else if (head.value.equals(":r")) {
                return firstOfClause(body.children.get(1),clauses);
            } else {
                System.err.println("Story.firstOfClause: unrecognized body head: " + head);
                return null;
            }
        } else {
            System.err.println("Story.firstOfClause: unrecognized body: " + body);
            return null;            
        }
    }

    
    public boolean loadTemplate(LispNode tn) {
        LispNode name = tn.children.get(0);
        List<HUDCUDLink> openLinks = new LinkedList<HUDCUDLink>();
        List<List<LispNode>> stack = new LinkedList<List<LispNode>>();
        List<LispNode> current = null;
        String text = "";
        Template t = new Template(name.value,"",0);
        
        List<LispNode> tmp = new LinkedList<LispNode>();
        for(int i = 1;i<tn.children.size();i++) tmp.add(tn.children.get(i));
        stack.add(tmp);
        
        while(!stack.isEmpty()) {
            current = stack.remove(0);
            while(!current.isEmpty()) {
                LispNode c = current.remove(0);
                if (c.type==LispNode.TYPE_STRING) {
                    text += c.value;
                } else if (c.type == LispNode.TYPE_LIST) {
                    LispNode objName = c.children.get(0);
                    CUDObject obj = getCUDObject(objName.value);
                    if (obj==null) {
                        System.err.println("Story.loadTemplate: cannot find object " + objName.value + " while parsing template: " + t);
                        return false;                        
                    }
                    HUDCUDLink link = new HUDCUDLink(t, text.length(), -1, obj);
                    t.links.add(link);
                    openLinks.add(0,link);
                    List<LispNode> tmp2 = new LinkedList<LispNode>();
                    for(int i = 1;i<c.children.size();i++) tmp2.add(c.children.get(i));
                    stack.add(0,current);
                    current = tmp2;
                } else {
                    System.err.println("Story.loadTemplate: error parsing template " + tn);
                    return false;
                }
            }
            if (openLinks.size()>0) {
                HUDCUDLink link = openLinks.remove(0);
                link.m_end = text.length();
            }
        }
        
        t.text = text;
        templates.add(t);
        
        return true;
    }


    public boolean loadStructure(LispNode discourse) {
        for(LispNode phase:discourse.children) {
            if (phase.type==LispNode.TYPE_LIST) {
                // phases!
                LispNode entities_n = phase.searchSymbol(":entities").parent;
                LispNode expressions_n = phase.searchSymbol(":expressions").parent;
                if (entities_n!=null) {
                    for(LispNode entity:entities_n.children) {
                        if (entity.type==LispNode.TYPE_LIST) {
                            // entity!
                            LispNode name = entity.children.get(0);
                            LispNode parent = null;
                            if (entity.children.size()>=3) parent = entity.children.get(2);
                            
                            if (name==null) {
                                System.err.println("Story.loadStructure: Can't parse entity: " + entity);
                                return false;
                            }
                            
                            Entity e = null;
                            if (parent==null) e = Entity.newEntity(name.value,null,this);
                                         else e = Entity.newEntity(name.value,parent.value,this);
                            if (e==null) {
                                System.err.println("Story.loadStructure: Can't create entity (" + name.value + " :type " + parent.value + ")");
                                return false;                                
                            }
                            entities.add(e);
                        }
                    }                    
                }
                if (expressions_n!=null) {
                    for(LispNode expression:expressions_n.children) {
                        if (expression.type==LispNode.TYPE_LIST) {
                            // expression!
                            LispNode expContent = expression.children.get(0);
                            LispNode name = expression.children.get(2);
                            
                            if (expContent == null || name==null) {
                                System.err.println("Story.loadStructure: Can't parse expression: " + expression);
                                return false;
                            }
                            
                            List<CUDObject> children = new LinkedList<CUDObject>();
                            for(int i = 1;i<expContent.children.size();i++) {
                                LispNode cn = expContent.children.get(i);
                                if (cn.type!=LispNode.TYPE_SYMBOL) {
                                    System.err.println("Story.loadStructure: Can't parse expression: " + expression);
                                    return false;                                    
                                }
                                CUDObject o = getCUDObject(cn.value);
                                if (o==null) {
                                    System.err.println("Story.loadStructure: CUDObject " + cn.value + " undefined while parsing expression: " + expression);
                                    return false;                                                                        
                                }
                                children.add(o);
                            }
                            Expression e = new Expression(name.value,expContent.children.get(0).value,children);
                            expressions.add(e);
                        }
                    }
                    
                }
            }
        }
        
        return true;
    }

}
