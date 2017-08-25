var relNos = {};
var senseId = -1;

function addrel(context, id, type="", target="") {
    relNos[id]++;
    var rn = relNos[id];
    if(isNaN(rn)) { rn = 0; }
    var cont = `<tr id="relType{{id}}-{{relNo}}-row">
                                <td>                     
                        <select class="s2-basic-{{id}}-{{relNo}}"
                               id="relType{{id}}-{{relNo}}"
                               name="relType{{id}}-{{relNo}}"
                               value="{{type}}">
                            <option value="hypernym">Hypernym (broader)</option>
                            <option value="hyponym">Hyponym (narrower)</option>
                            <option value="instance_hypernym">Instance of (hypernym)</option>
                            <option value="instance_hyponym">Has instance (hyponym)</option>
                            <option value="antonym">Antonym (opposite)</option>
                            <option value="emotion">Shows emotion</option>
                            <option value="derivation">Derived from (linguistically)</option>
                            <option value="loanword">Loanword from this language</option>
                            <!--<option value="involved">Involved in</option>-->
                            <option value="also">See also</option>
                            <option value="causes">Causes</option>
                            <option value="domain_region">Region</option>
                            <option value="domain_topic">Topic</option>
                            <option value="domain_usage">Exemplifies</option>
                            <option value="similar">Similar to</option>
                            <option value="mero_location">Meronym location (sub-location)</option>
                            <option value="mero_member">Meronym member (is member of)</option>
                            <option value="mero_part">Meronym part (is component of)</option>
                            <option value="mero_portion">Meronym portion (is division of)</option>
                            <option value="mero_substance">Meronym substance (is made of)</option>
                            <option value="holo_location">Holonym location (containing location)</option>
                            <option value="holo_member">Holonym member (is group of)</option>
                            <option value="holo_part">Holonym part (is composed of)</option>
                            <option value="holo_portion">Holonym portion (divides into)</option>
                            <option value="holo_substance">Holonym substance (substance of)</option>
                            <option value="pejorative">Pejorative for</option>
                        </select>
                        <script>$('#relType{{id}}-{{relNo}}').val('{{type}}');</script>
                                </td>
                                <td>
                        <div class="ui-widget">
                        <input type="text" class="form-control"
                               id="relTarget{{id}}-{{relNo}}"
                               name="relTarget{{id}}-{{relNo}}"
                               value="{{target}}"/>
                        </div>
                        </td>
                        <td>
                        <button type="button" class="btn btn-danger" onclick="$('#relType{{id}}-{{relNo}}-row').remove();return false">Remove</button>
                                </td>
                            </tr>`;
    var last_cont = "";
    while(last_cont !== cont) {
        last_cont = cont;
        cont = cont.replace("{{id}}", id);
        cont = cont.replace("{{relNo}}", rn);
    }
    cont = cont.replace("{{type}}", type);
    cont = cont.replace("{{type}}", type);
    cont = cont.replace("{{target}}", target);
    $("#relTable" + id).append(cont);
    relNos[id] = rn;
    wncomplete('#relTarget' + id + "-" + rn,context);
    $('.s2-basic-' + id + "-" + rn).select2().on("change", function(e) {
        if($('.s2-basic-'+id+'-'+rn).val() == "none") {
            $('#relType'+id+'-'+rn+'-row').remove();
        }
    });
    $('#relType' + id + "-" + rn).focus();
}

function addsense(context, pos="",synonym="",definition="",abbrev="",misspell="") {
    var code = `<span id="sense{{id}}">
                <div class="panel panel-info cwn-sense">
                    <div class="panel-heading">Sense {{id}}
                        <button type="button" class="close pull-right" onclick="removesense({{id}});return false">&times;</button>
                    </div>
                    <div class="panel-body">
                    <div class="form-group cwn-sense">
                        <label for="pos{{id}}">Part of Speech
                        <button class="btn-xs btn btn-info" onclick="$('#pos{{id}}-help').toggle();" tabindex="-1">Help</button></label><br/>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="n" onclick="$('#synonym{{id}}').focus();"/>Noun</label></div>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="v" onclick="$('#synonym{{id}}').focus();"/>Verb</label></div>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="a" onclick="$('#synonym{{id}}').focus();"/>Adjective</label></div>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="r" onclick="$('#synonym{{id}}').focus();"/>Adverb</label></div>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="x" onclick="$('#synonym{{id}}').focus();"/>Other</label></div>
                        <div id="pos{{id}}-help" class="cwn-help">
                            <p>Please give the part-of-speech associated with this sense of the term or phrase. 
                            "Other" is used for interjections such as "wow!" or "gosh!".</p>
                        </div>
                    </div>
                     <div class="form-group cwn-sense">
                        <label for="synonym{{id}}">Synonym
                        <button class="btn-xs btn btn-info" onclick="$('#synonym{{id}}-help').toggle();return false" tabindex="-1">Help</button></label>
                        <input type="text" class="form-control"
                               id="synonym{{id}}" name="synonym{{id}}" value="{{synonym}}" oninput="checkDefnSyn({{id}})"/>
                        <div id="synonym{{id}}-help" class="cwn-help">
                            <p>Please first type in suitable synonym terms to find any likely terms that are already in Colloquial or Princeton WordNet. If you find a suitable term select it from the drop-down, otherwise leave <b>Synonym</b> blank and enter a definition below.</p>
                        </div>
                    </div>
                     <div class="form-group cwn-sense">
                        <label for="definition{{id}}">Definition
                        <button class="btn-xs btn btn-info" onclick="$('#definition{{id}}-help').toggle();return false" tabindex="-1">Help</button></label>
                        <input type="text" class="form-control"
                               id="definition{{id}}" name="definition{{id}}" value="{{definition}}" oninput="checkDefnSyn({{id}})"/>
                        <div id="definition{{id}}-help" class="cwn-help">
                            <p>(only if you cannot find a synonym term) Please give a definition of this term that describes it well. A good definition consists of a <i>genus</i> and a <i>differentia</i>. The genus is the type of thing that a word is e.g., "parkour" is a "training discipline", and the differentia is a criteria that unambiguously identifiers this word, e.g., "using movement that developed from military obstacle course training". It is advised that you use definitions from other sources and if you do so please include the source in square brackets after the definition, e.g., "A training discipline using movement that developed from military obstacle course training [Wikipedia]".</p>
                        </div>
                    </div>
                    <div class="alert alert-danger" id="syndefn{{id}}" style="display:none;">
                        Error: please set either the synonym or the definition (not both)!
                    </div>
                    <div class="form-group cwn-sense">
                        <table style="width:100%;" id="relTable{{id}}">
                            <tr>
                                <th style="margin-right:10px;">Relation
                        <button class="btn-xs btn btn-info" onclick="$('#relation{{id}}-help').toggle();return false" tabindex="-1">Help</button>
                        <button class="btn btn-xs btn-success" type="button" onclick="addrel('{{context}}',{{id}})">Add</button>
                                </th>
                                <th>Target</th>
                            </tr>
                        <table>
                        <div id="relation{{id}}-help" class="cwn-help">
                            <p>(only if you cannot find a synonym term) Please give at least one relation linking this term to an existing term in Colloquial or Princeton WordNet by typing the word under target and selecting the synset from the autocomplete. In general, it is expected that nouns always have a hypernym (broader) term, verbs, adjectives and adverbs may have a broader term and/or a similar term. Please also consider the origin of the term: if it is derived from an existing word, please add a "derived from" link, if this word is borrowed from another language, please add a loanword linking, whose target is the synset for the language this word is borrowed from.</p>
                            <ul>
                                <li><b>Hypernym (broader):</b> A target term whose meaning subsumes this one, most terms should have one broader term. A term may have more than one hypernym, but please add only <b>distinct</b> <b>direct</b> hypernyms, e.g., a <i>pike</i> is a <i>fish</i> and a <i>predator</i>, but do not add <i>animal</i> (fish and animal are not distinct hypernyms of pike).</li>
                                <li><b>Hyponym (broader):</b> A target term whose meaning is subsumed by this one, it is not generally necessary to add this</li>
                                <li><b>Instance of (hypernym):</b> This term is an instance of the class given by the selected target, this should only be used for proper nouns, which should only be defined in special cases</li>
                                <li><b>Has instance (hyponym):</b> This should rarely be used</li>
                                <li><b>Antonym (opposite):</b> A term with the opposite meaning, mostly used for adjectives, e.g., "hot" vs. "cold"</li>
                                <li><b>Shows emotion:</b> This is used for interjections that show a particular emotion, e.g., "wow!" shows the emotion of "surprise"</li>
                                <li><b>Loanword for this language:</b> If this word is borrowed from another language, use this property linked to the synset for the language where the word is borrowed from</li>
                                <li><b>Derived from (linguistically):</b> This word is derived from another word, e.g., "shorty" from "short", or is a multiword term whose elements are the chosen words</li>
                                <li><b>See also:</b> Used for relevant relations which do not fit under any other category</li>
                                <li><b>Causes:</b> Something that is the result of the event described by this noun or the action of this verb</li>
                                <li><b>Region:</b> The region that this is used in. This also indicates dialect, e.g. select "United Kingdom" for slang terms used in the UK. For African-American Vernacular English, please use AAVE as the region</li>
                                <li><b>Topic:</b> A domain in which this word is used, e.g., "Medicine" for medical terms</li>
                                <li><b>Exemplifies:</b> If this word is of a particular terminological type, e.g., "insult", "slang", "colloquialism", "trade mark"</li>
                                <li><b>Similar to:</b> This word is similar, but not equal, in meaning to another word. This should be used if a suitable hypernym cannot be discovered</li>
                                <li><b>Meronym/Holonym:</b> This word is a part (meronym) or is made of (holonym) something else. There are five subcategories: 
<ul>
    <li>"Paris" is a location meronym of "France"</li>
    <li>"Squirrel" is a member meronym of "family Sciuidae"</li>
    <li>"Transmission" is a part meronym of "automobile"</li>
    <li>"Slice" is a portion meronym of "cake"</li>
    <li>"Ivory" is a substance meronym of "tusk"</li>
</ul></li>
                                <li><b>Pejorative for:</b> This term is a negative term used to describe something (typically a group of people). The target is the object or group being described</li>
                            </ul>
                            <p>You can delete a relation by clicking the small "x" to it's right.</p>
                        </div>

                    </div></div></div></span>`;
    var id = Object.keys(relNos).length + 1;
    relNos[id] = 0;
    var last_code = "";
    while(last_code !== code) {
        last_code = code;
        code = code.replace("{{id}}", id);
    }
    code = code.replace("{{synonym}}", synonym);
    code = code.replace("{{definition}}", definition);
    code = code.replace("{{abbrev}}", abbrev);
    code = code.replace("{{misspell}}", misspell);
    code = code.replace("{{context}}", context);
    $(code).insertBefore('#submitDiv');
    if(pos !== "") {
        $("input:radio[name=pos" + id + "][value=" + pos + "]").attr("checked", true);
    }
    wncomplete('#synonym' + id, context);
    $('#sense' + id + " .cwn-sense").show();
    $("input:radio[name=pos"+id+"][value=n]").focus();
}

function removesense(id) {
    $('#sense' + id).remove();
}
 
function setConfidence(value) {
    if(!$('#confidence-' + value).attr("checked")) {
        $('#confidence-' + value).attr("checked", true);
    }
    switch(value) {
        case 'vstrong':
        case 'strong':
        case 'medium':
        case 'weak':
            $('.cwn-status').show();
            var status_val = $('input:radio[name="status"]:checked').val();
            if(status_val) {
                setStatus(status_val,"");
            }
            $('#status-general').focus();
            break;
        case 'skip':
        case '':
            $('.cwn-status').hide();
            $('.cwn-sense').hide();
            $('.cwn-abbrev').hide();
            $('.cwn-misspell').hide();
            $('.cwn-inflected').hide();
            break;            
    }
}

function add_misspell_or_abbrev(type, value="") {
    var id = $('.' + type + '-input').size() + 1;
    var code = "<tr id='" + type + id + "-row'><td width=\"100%\"><input type='text' class='form-control " + type + "-input' id='" + type + id + 
        "' name='" + type + id + "' value='" + value.replace(/'/g, "&quot;") + "'/>"
        + "</td><td><button type='button' class='close' onclick='remove_misspell_or_abbrev(" +
        id + ",\"" + type + "\");return false'>&times;</button></td></tr>";
    $(code).insertBefore('#' + type + 's');
}

function remove_misspell_or_abbrev(id, type) {
    $('#' + type + id + "-row").remove();
}

function setStatus(value, context=null) {
    if(!$('#status-' + value).attr("checked")) {
        $('#status-' + value).attr("checked", true);
    }
    switch(value) {
        case 'general':
        case 'novel':
        case 'vulgar':
            $('.cwn-sense').show();
            $('.cwn-abbrev').hide();
            $('.cwn-misspell').hide();
            $('.cwn-inflected').hide();
            if($('#sense1').size() == 0 && context !== null) {
                addsense(context);
            }
            break;
        case 'abbrev':
            $('.cwn-sense').hide();
            $('.cwn-misspell').hide();
            $('.cwn-inflected').hide();
            $('.cwn-abbrev').show();
            break;
        case 'misspell':
            $('.cwn-sense').hide();
            $('.cwn-abbrev').hide();
            $('.cwn-inflected').hide();
            $('.cwn-misspell').show();
            break;
        case 'inflected':
            $('.cwn-sense').hide();
            $('.cwn-abbrev').hide();
            $('.cwn-inflected').show();
            $('.cwn-misspell').hide();
            break;
        case 'name':
        case 'nonlex':
        case 'error':
            $('.cwn-sense').hide();
            $('.cwn-abbrev').hide();
            $('.cwn-misspell').hide();
            $('.cwn-inflected').hide();
            break;
    }
}

function checkDefnSyn(id) {
    if($('#synonym'+id).val() !== "" &&
       $('#definition'+id).val() !== "") {
        $('#syndefn'+id).show();
    } else {
        $('#syndefn'+id).hide();
    }
}

function clearSyn(id) {
    $('#synonym'+id).val("");
}

