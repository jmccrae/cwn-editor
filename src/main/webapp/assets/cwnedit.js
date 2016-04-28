var relNos = {};
var senseId = -1;

function addrel(id) {
    relNos[id]++;
    var rn = relNos[id];
    var cont = `<tr>
                                <td>                     
                        <select class="s2-basic-{{id}}-{{relNo}}"
                               id="relType{{id}}-{{relNo}}"
                               name="relType{{id}}-{{relNo}}"
                               value="{{type}}">
                            <option value="hypernym">Hypernym (broader)</option>
                            <option value="hyponym">Hypernym (narrower)</option>
                            <option value="instance_hypernym">Instance of (hypernym)</option>
                            <option value="instance_hyponym">Has instance (hyponym)</option>
                            <option value="antonym">Antonym (opposite)</option>
                            <option value="emotion">Shows emotion</option>
                            <option value="derivation">Derived from (linguistically)</option>
                            <option value="loanword">Loanword from this language</option>
                            <option value="involved">Involved in</option>
                            <option value="also">See also</option>
                            <option value="causes">Causes</option>
                            <option value="domain_region">Region</option>
                            <option value="domain_topic">Topic</option>
                            <option value="domain_usage">Usage</option>
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
                        </select>
                                </td>
                                <td>
                        <div class="ui-widget">
                        <input type="text" class="form-control"
                               id="relTarget{{id}}-{{relNo}}"
                               name="relTarget{{id}}-{{relNo}}"
                               value=""/>
                        </div>
                                </td>
                            </tr>`;
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{relNo}}", rn);
    cont = cont.replace("{{relNo}}", rn);
    cont = cont.replace("{{relNo}}", rn);
    cont = cont.replace("{{relNo}}", rn);
    cont = cont.replace("{{relNo}}", rn);
    $("#relTable" + id).append(cont);
    relNos[id] = rn;
    wncomplete('#relTarget' + id + "-" + rn);
    $('.s2-basic-' + id + "-" + rn).select2();
}

function addsense() {
    var id = Object.keys(relNos).length + 1;
    relNos[id] = 0;
    var cont = `<h3>Sense {{id}}</h3>
                    <div class="form-group">
                        <label for="pos{{id}}">Part of Speech</label>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="n"/>Noun</label></div>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="v"/>Verb</label></div>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="a"/>Adjective</label></div>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="r"/>Adverb</label></div>
                        <div class="radio"><label>
                        <input type="radio" name="pos{{id}}"
                            value="x"/>Other</label></div>
                    </div><div class="form-group">
                        <label for="definition{{id}}">Definition</label>
                        <input type="text" class="form-control"
                               id="definition{{id}}" name="definition{{id}}" value=""/>
                    </div>
                    <div class="form-group">
                        <label for="synonym{{id}}">Synonym</label>
                        <input type="text" class="form-control"
                               id="synonym{{id}}" name="synonym{{id}}" value=""/>
                    </div>
                    <div class="form-group">
                        <table style="width:100%;" id="relTable{{id}}">
                            <tr>
                                <th style="margin-right:10px;">Relation</th>
                                <th>Target</th>
                            </tr>
                        </table>
                    </div>
                    <div>
                        <button class="btn btn-success" type="button" onclick="addrel({{id}})">Add relation</button>
                    </div>`;
    var last_cont = "";
    while(last_cont !== cont) {
        last_cont = cont;
        cont = cont.replace("{{id}}", id);
    }
    $(cont).insertBefore('#submitDiv');
}
 

