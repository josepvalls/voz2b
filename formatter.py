'''
New formatting architecture:
    The formatter is a lambda that received the element to be formatted, the formatter and the features to format
    The formatter will then select a subformatter based on the element to be formatted and forward the arguments
    The element to be formatted can format subelements using the received formatter
'''
from nltk.tree import ParentedTree
import voz,util
import logging
import types,cgi

logger = logging.getLogger(__name__)

'''Not in use
class VozFormatter(object):
    def wrap(self,string,wrapper='',options={}):
        return self.wrap_prefix(wrapper,options)+string+self.wrap_postfix(wrapper,options)
    def enumerate(self,lst,glue='',wrapper_list='',wrapper_item='',options={}):
        """
        :param lst: list[VozTextContainer]
        :param wrapper_list: str
        :param wrapper_item: str
        :return: str
        """
        return self.__enumerate(lst,
                                glue or self.default_glue(),
                                wrapper_list or self.default_wrapper_list(),
                                wrapper_item or self.default_wrapper_item(),
                                options)
    def __enumerate(self,lst,glue='',wrapper_list='',wrapper_item='',options={}):
        options_new = {'pre':'  '+options.get('pre','')}
        options_new.update(options)
        return self.wrap(
            glue.join([self.wrap(item.format(self,options_new),wrapper_item) for item in lst]),
            wrapper_list)
    def wrap_prefix(self,wrapper='',options={}):
        return ''
    def wrap_postfix(self,wrapper='',options={}):
        return ''
    def default_glue(self):
        return ''
    def default_wrapper_list(self):
        return ''
    def default_wrapper_item(self):
        return ''

class TextFormatter(VozFormatter):
    def wrap_prefix(self,wrapper='',options={}):
        return options.get('pre','')+wrapper
    def wrap_postfix(self,wrapper,options={}):
        return options.get('post','\n')
    def default_wrapper_item(self):
        return '*'


class HtmlFormatter(VozFormatter):
    def wrap_prefix(self,wrapper='div',options={}):
        return options.get('pre','')+'<%s>'%wrapper
    def wrap_postfix(self,wrapper='div',options={}):
        return ('</%s>'%wrapper)+options.get('post','')
    def default_wrapper_list(self):
        return 'ul'
    def default_wrapper_item(self):
        return 'li'

text_formatter = TextFormatter()
html_formatter = HtmlFormatter()
'''



class VozHTMLFormatter(object):
    formatter_templates = [
        (ParentedTree,'format_tree'),
        (voz.Graph, 'format_story_graph'),
        (list,'format_list'),
        (set, 'format_list'),
        (voz.Token,'format_token'),
        (voz.Sentence,'format_sentence'),
        (voz.Document,'format_document'),
        (voz.entitymanager.Mention,'format_mention'),
        (voz.verbmanager.Verb,'format_verb'),
        (str,'format_str'),
        (types.NoneType,'format_none'),
        (object,'wrap'),

    ]
    @classmethod
    def format_str(cls,s,options={}):
        return cgi.escape(s)
    @classmethod
    def format_none(cls,element,options={}):
        return cls.wrap(cls.wrap(cls.format_str('<None>'),'pre'),'div')
    @classmethod
    def wrap(cls,i,tag='div',options={}):
        extra = ''
        if options.get('class',None): extra += ' class=' + options['class']
        if options.get('id', None): extra += ' id=' + options['id']
        return '<%s%s>%s</%s>' % (tag, extra,str(i),tag) if tag else str(i)
    @classmethod
    def html(cls,payload,list_wrapper='p',container_wrapper='div'):
        if isinstance(payload,list):
            payload = '\n'.join([cls.wrap(i,list_wrapper) for i in payload])
        payload = cls.wrap(payload,container_wrapper)
        return '<html><body>%s</body></html>' % str(payload)
    @classmethod
    def format_list(cls,lst,glue='',options={}):
        return glue.join([cls.format(i,options) for i in lst])
    @classmethod
    def format_node(cls,node,options={}):
        if options and 'color_map' in options:
            color_map = options['color_map']
        else:
            color_map = {}
        if node:
            css = 'pos' if isinstance(node,ParentedTree) else 'term'
            if isinstance(node,ParentedTree) and tuple(node.treeposition()) in color_map:
                css +=' e'+str(color_map[tuple(node.treeposition())])
            return '<div><span class="%s">%s<span>%s</div>' % (
                css,
                node.label() if isinstance(node,ParentedTree) else cls.format(node),
                (''.join([cls.format_node(i,options) for i in node]) if isinstance(node,ParentedTree) else '')
            )
    @classmethod
    def format_tree(cls,nodes,options={}):
        return '<div class="text-container parse-container">%s</div>' % ''.join([cls.format_node(i,options) for i in nodes])
    @classmethod
    def format_token(cls,token,options={}):
        return cls.wrap(token.get_text()+'<br/>'+cls.wrap(token.id,'small'),'span')
    @classmethod
    def format_document(cls,document,options={}):
        return cls.format(document.sentences,options=options)
    @classmethod
    def format_sentence(cls,sentence,options={}):
        out = ''
        if options.get('include_parse',False)==True:
            if options and 'parse_highlight' in options:
                options['color_map'] = sentence.parse_highlight.get(options['parse_highlight'],{})
            out += cls.format(sentence.parse_tree,options)
        if options.get('include_raw',False)==True:
            out += '<div class="text-container raw-container"><pre>%s</pre></div>' % sentence.get_text()
        if options.get('include_mentions',False)==True:
             for mention in sentence.mentions:
                if mention.is_independent:
                    out += cls.format_mention_extra(mention)
        if options.get('include_verbs',False)==True:
             for verb in sentence.verbs:
                out += cls.format_verb_extra(verb, options)

        if options.get('include_text',False)==True:
            to_highlight = []
            for mention in sentence.mentions:
                if mention.is_independent and mention not in to_highlight:
                    to_highlight.append(mention)
            for verb in sentence.verbs:
                if verb not in to_highlight:
                    to_highlight.append(verb)
            to_highlight.sort(key=lambda i:(i.get_tokens()[0].offset,len(i.get_tokens())))
            p_start = sentence.offset
            out_ = ''
            included_mentions = [] #type: list[entitymanager.Mention]
            while True:
                if p_start >= sentence.offset+sentence.len:
                     break # we finished with the sentence
                elif to_highlight and to_highlight[0].get_tokens()[0].offset <= p_start:
                    item = to_highlight.pop(0)
                    for mention2 in included_mentions:
                        if isinstance(item,voz.entitymanager.Mention) and mention2.contains_mention(item):
                            out_ += ' ('+cls.format(item)+')'
                            item = mention2
                            break
                        else:
                            pass
                    else:
                        out_ += cls.format(item)

                    if isinstance(item,voz.entitymanager.Mention) and item not in included_mentions:
                        included_mentions.append(item)

                    p_start = item.get_tokens()[-1].offset+item.get_tokens()[-1].len
                elif to_highlight and to_highlight[0].get_tokens()[0].offset > p_start:
                    out_ += sentence._parent_document.get_text()[p_start:to_highlight[0].get_tokens()[0].offset]
                    p_start = to_highlight[0].get_tokens()[0].offset
                else:
                    out_ += sentence._parent_document.get_text()[p_start:sentence.offset+sentence.len]
                    break

            out += '<div class="text-container raw-container">%s</div>' % out_

        return out
    @classmethod
    def format_mention_extra(cls, mention, options = {}):
        return cls.format_mention(mention,options)
    @classmethod
    def format_mention(cls,mention,options={}):
        color_class = ' e%d' % mention.get_coref_group_id()
        return '<span class="tooltip%s" title="%s"><u>%s</u></span>' % (color_class,str(mention),mention.get_text())
    @classmethod
    def format_verb_extra(cls, verb, options={}):
        return '<div><div style="float: left;">%s: </div> <div style="float: left"><div>%s</div> <div>%s</div></div></div><div style="clear: both"/>' % (
            cls.format_verb(verb,options),
            cls.format_list([verb.get_subjects() or 'N/A'],'; '),
            cls.format_list([verb.get_objects() or 'N/A'], '; ')
        )
    @classmethod
    def format_verb(cls,verb,options={}):
        color_class = ''
        return '<span class="tooltip%s" title="%s"><u>%s</u></span>' % (color_class,str(verb),verb.get_text())

    @classmethod
    def format(cls,element,options={}):
        for t,formatter in cls.formatter_templates:
            if isinstance(element,t):
                logger.debug("Formatting using %s" % formatter)
                return getattr(cls,formatter)(element,options=options)
        return cls.wrap(str(element))

    @classmethod
    def format_story_graph(cls, graph, options={}):
        nodes, vertices = graph
        include_na = options.get('include_na', True)
        verb_glue =  options.get('verb_glue', ', ')
        out = ''
        table = []
        for i in nodes:
            table.append([i.predictions.coref, i.predictions.type, i.predictions.role, cls.format(i)])
        out += cls.html_table_from_list_of_lists(table)
        nodes_ = [i.predictions.coref for i in nodes]
        if include_na:
            nodes_.append(-1)
        table = []
        table.append(['']+nodes_)
        for sub in nodes_:
            row = [sub]
            for obj in nodes_:
                row.append(verb_glue.join([str(i.get_text()) for i in vertices.get((sub, obj), [])]))
            table.append(row)
        out += cls.html_table_from_list_of_lists(table)
        return out
    @classmethod
    def html_table_from_list_of_lists(cls, table, options={}):
        separator = '\n'
        return html_wrap('table',
            separator.join([html_wrap('tr',
            separator.join([html_wrap('td',i) for i in row])) for row in table]))

def html_wrap(tag,contents):
    return '<%s>%s</%s>' % (tag,contents,tag.split()[0])




def html(body,pre=''):
    return '''<html><head>
        <link rel="stylesheet" type="text/css" href="css/colResizable.css" />
        <script type="text/javascript" src="js/jquery-1.9.1.min.js"></script>

        <script type="text/javascript">
            this.tooltip = function(){
                /* CONFIG */
                    xOffset = 10;
                    yOffset = 20;
                    // these 2 variable determine popup's distance from the cursor
                    // you might want to adjust to get the right result
                /* END CONFIG */
                $("span.tooltip").hover(function(e){
                    this.t = this.title;
                    this.title = "";
                    $("body").append("<p id='tooltip'>"+ this.t +"</p>");
                    $("#tooltip")
                        .css("top",(e.pageY - xOffset) + "px")
                        .css("left",(e.pageX + yOffset) + "px")
                        .fadeIn("fast");
                },
                function(){
                    this.title = this.t;
                    $("#tooltip").remove();
                });
                $("span.tooltip").mousemove(function(e){
                    $("#tooltip")
                        .css("top",(e.pageY - xOffset) + "px")
                        .css("left",(e.pageX + yOffset) + "px");
                });
            };



            // starting the script on page load
            $(document).ready(function(){
                tooltip();
            });
        </script>
        <style>%s</style></head><body">%s

        <pre style="display:none">\n%s\n</pre></body></html>''' % (css(),body,pre)
def css():
    css='''
#tooltip{
    position:absolute;
    border:1px solid #333;
    background:#f7f5d1;
    padding:2px 5px;
    color:#333;
    display:none;}
div#navmenu { background-color: #ddd; width: 120px;
    float: right;
    right: 0px;
    position: fixed;
    top: 0px;
    font-size: 80% }
div#navmenu ol { margin-left: 2em; padding-left: 0px }
div#navmenu ol ol { margin-left: 0.6em; padding-left: 0px list-style-type: upper-alpha; }
td {overflow:hidden; height: 30px }
div.parse-container span {
display: block;
text-align: center;
padding: 2px;
padding-bottom: 0;
}
div.parse-container div {
border: 1px solid black;
border-bottom: 0 solid black;
display: inline-block;
margin: 2px;
margin-bottom: 0;
}
div.text-container {
display: block;
width: auto;
overflow: scroll;
white-space: nowrap;
border-width: 0
}
div.entity-container div.entity {
display: inline-block;
vertical-align: top;
width: 300px;
height: 300px;
overflow: scroll;
}
div.desc {
font-style: italic;
}
span.dt {
font-weight: bold;
}
span.entity-name {
font-size:120%;
}
.error { color: red }
.blue { background-color: blue }
.d-2 {background-color: rgb(67,76,67) }
.e-1 {background-color: rgb(181,181,168) }
.ner { border: 1px solid red}
'''
    css += '\n'.join(['.e%d { background-color:%s;}' % (i,v)for i,v in enumerate(['rgb(85,135,192)', 'rgb(91,87,166)', 'rgb(120,84,164)', 'rgb(189,74,160)', 'rgb(227,54,83)', 'rgb(245,99,65)', 'rgb(245,151,65)', 'rgb(245,191,67)', 'rgb(246,232,69)', 'rgb(211,221,79)', 'rgb(119,195,92)', 'rgb(91,197,149)']*300)])
    #'rgb(67,76,67)', 'rgb(181,181,168)'
    return css
