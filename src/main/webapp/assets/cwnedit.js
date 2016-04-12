var relNos = {};
var senseId = -1;

function addrel(id) {
    relNos[id]++;
    var rn = relNos[id];
    var cont = `<tr> 
                                <td> 
                        <input type="text" class="form-control"
                               id="relType{{id}}-{{relNo}}"
                               name="relType{{id}}-{{relNo}}"
                               value=""/>
                                </td>
                                <td>
                        <input type="text" class="form-control"
                               id="relTarget{{id}}-{{relNo}}"
                               name="relTarget{{id}}-{{relNo}}"
                               value=""/>
                                </td>
                            </tr>`;
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{id}}", id);
    cont = cont.replace("{{relNo}}", rn);
    cont = cont.replace("{{relNo}}", rn);
    cont = cont.replace("{{relNo}}", rn);
    cont = cont.replace("{{relNo}}", rn);
    $("#relTable" + id).append(cont);
}
