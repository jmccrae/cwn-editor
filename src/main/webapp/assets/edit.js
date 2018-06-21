
function toggleSynonymHelp(id) {
    $('#synonym'+id + '-help').toggle();
    return false;
}
function toggleDefinitionHelp(id) {
    $('#definition'+id + '-help').toggle();
    return false;
}
function togglePosHelp(id) {
    $('#pos'+id + '-help').toggle();
    return false;
}
function toggleRelationHelp(id) {
    $('#relation'+id + '-help').toggle();
    return false;
}


angular.module('app', ['ngMaterial']);

angular.module('app').controller('CWNEditController', function($scope, $http) {
    $scope.entry = entry();
    $scope.entry.abbrevs=[{"text": ""}];
    $scope.entry.misspells=[{"text": ""}];
    $scope.entry.inflecteds=[{"text": ""}];
    if($scope.entry.senses.length == 0) {
        $scope.entry.senses.push({
            "synonym": "",
                "synset": {
                "pos": "",
                "definition": ""
            },
            relations: [],
            id: 1
        });
    }
    if($scope.entry.status === 'aux') {
        $scope.entry.status = 'general';
    }

    if($scope.entry.status === 'general' ||
            $scope.entry.status === 'novel' ||
            $scope.entry.status === 'vulgar') {
        for(sense in $scope.entry.senses) {
            var ss = $scope.entry.senses[sense].synset;
            $scope.entry.senses[sense].id = (parseInt(sense) + 1);
            for(r in synsets[ss].relations) {
                var relation = synsets[ss].relations[r];
                $scope.entry.senses[sense].relations.push(relation);
            }
            delete $scope.entry.senses[sense].synset.relations;
            if(ss.match("[ip]\\d+")) {
                $scope.entry.senses[sense].external = true;
            }
            $scope.entry.senses[sense].synset = synsets[ss];
        }
    } else if($scope.entry.status === 'abbrev') {
        $scope.entry.abbrevs = $scope.entry.senses.map(function(s) {
            return {"text": s.synset, "new": false};
        });
        $scope.entry.senses = [];
    } else if($scope.entry.status === 'misspell') {
        $scope.entry.misspells = $scope.entry.senses.map(function(s) {
            return {"text": s.synset, "new": false};
        });
        $scope.entry.senses = [];
    } else if($scope.entry.status === 'inflected') {
        $scope.entry.inflecteds = $scope.entry.senses.map(function(s) {
            return {"text": s.synset, "new": true};
        });
        $scope.entry.senses = [];
    } 

    $scope.toggleSynonymHelp = function(id) {
        $('#synonym'+id + '-help').toggle();
        return false;
    };
    $scope.toggleDefinitionHelp = function(id) {
        $('#definition'+id + '-help').toggle();
        return false;
    };
    $scope.togglePosHelp = function(id) {
        $('#pos'+id + '-help').toggle();
        return false;
    };
    $scope.toggleRelationHelp = function(id) {
        $('#relation'+id + '-help').toggle();
        return false;
    };
    $scope.addSense = function() {
        var nextId = 1;
        for(nextId = 1;;nextId++) {
            var found = false;
            for(id2 in $scope.entry.senses) {
                if(nextId === $scope.entry.senses[id2].id) {
                    found = true;
                }
            }
            if(!found) {
                break;
            }
        }
        $scope.entry.senses.push({
            "synonym": "",
            "synset": {
                "pos": "",
                "definition": ""
            },
            relations: [],
            id: nextId
        });
    };
    $scope.removeSense = function(id) {
        for(id2 in $scope.entry.senses) {
            if(id === $scope.entry.senses[id2].id) {
                $scope.entry.senses.splice(id2, 1);
            }
        }
    };
    $scope.addRel = function(sense) {
        sense.relations.push({
            "type": "hypernym",
            "trgWord": "",
            "trgSynset": ""
        });
    };
    $scope.removeRel = function(sense, relation) {
        for(r in sense.relations) {
            var tRel = sense.relations[r];
            if(tRel.type === relation.type &&
               tRel.target === relation.target) {
                sense.relations.splice(r, 1);
                return;
            }
        }
    }
    $scope.addAbbrev = function() {
        $scope.entry.abbrevs.push({"text": ""});
    };
    $scope.removeAbbrev = function(abbrev) {
        for(a in $scope.entry.abbrevs) {
            var abbrev2 = $scope.entry.abbrevs[a];
            if(abbrev2.text === abbrev.text) {
                $scope.entry.abbrevs.splice(a,1);
                return;
            }
        }
    };
    $scope.addMisspell = function() {
        $scope.entry.misspells.push({"text": ""});
    };
    $scope.removeMisspell = function(misspell) {
        for(a in $scope.entry.misspells) {
            var misspell2 = $scope.entry.misspells[a];
            if(misspell2.text === misspell.text) {
                $scope.entry.misspells.splice(a,1);
                return;
            }
        }
    };
    $scope.addInflected = function() {
        $scope.entry.inflecteds.push({"text": ""});
    };
    $scope.removeInflected = function(inflected) {
        for(a in $scope.entry.inflecteds) {
            var inflected2 = $scope.entry.inflecteds[a];
            if(inflected2.text === inflected.text) {
                $scope.entry.inflecteds.splice(a,1);
                return;
            }
        }
    };
    $scope.changeInflected = function(inflected,idx) {
        return $http.get(contextUrl() + "/wn/" + inflected.text)
            .then(function(result) {
                    inflected.synonyms = result.data.map(function(s) {
                            var ili = s.match(".* <([icp]\\d+)>")[1];
                            var word = s.match("(.*) \\(.*\\):.*")[1];
                            return {"display": s, "ili": ili, "word": word};
                            }).filter(function(s) {
                                return s.word == inflected.text.toLowerCase();
                            })
                    });
 
    };
    $scope.submitEntry = function() {
        var data = $scope.entry;
        $http.post(contextUrl() + "/update/" + updateNext(), data).then(function() {
            window.location = contextUrl() + "/next_queue";
        }, function(response) {
            console.log("submitEntry failed:" + response.data + " (" + response.status + ")");
            $scope.error = response.data;
        });
        
    };
});

angular.module('app').controller('SynRelAutoComplete', function($http,$scope) {
        var self = this;
        self.querySearch   = querySearch;
        var display = "";
        self.searchText = "";
        self.items = [];
        if($scope.$parent.relation.trgSynset !== "") {
            if($scope.$parent.relation.trgWord === "" ||
                $scope.$parent.relation.trgWord === undefined) {
                self.selectedItem = {
                    "ili": $scope.$parent.relation.trgSynset,
                    "word": $scope.$parent.relation.trgWord,
                    "display": "<" + $scope.$parent.relation.trgSynset + ">"
                };

            } else {
                self.selectedItem = {
                    "ili": $scope.$parent.relation.trgSynset,
                    "word": $scope.$parent.relation.trgWord,
                    "display": $scope.$parent.relation.trgWord + ": <" +
                        $scope.$parent.relation.trgSynset + ">"
                };
            }
        }

        function querySearch () {
            return $http.get(contextUrl() + "/wn/" + self.searchText)
                .then(function(result) {
                    self.items = result.data.map(function(s) {
                        var ili = s.match(".* <([icp]\\d+)>")[1];
                        var word = s.match("(.*) \\(.*\\):.*")[1];
                        return {"display": s, "ili": ili, "word": word};
                    });
                    return self.items;
                });
        }

        self.selectItem = function(item) {
            if(item) {
               $scope.$parent.relation.trgSynset = item.ili;
                $scope.$parent.relation.trgWord = item.word;
                self.items = [];
            } else {
                self.clearItem();
            }
        }

        self.clearItem = function() {
                $scope.$parent.relation.trgSynset = "";
                $scope.$parent.relation.trgWord = "";
        };
    });

angular.module('app').controller('SynAutoComplete', function($http,$scope) {
        var self = this;
        self.querySearch = querySearch;
        self.searchText = "";
        if($scope.$parent.sense.external) {
            var ss = $scope.$parent.sense.synset;
            self.selectedItem = {
                "display": (ss.lemmas.join(",") + ": " + 
                    ss.definition + " <" + ss.id + ">"),
                "definition": ss.definition,
                "ili": ss.id,
                "pos": ss.pos
            };
        }
            

        function querySearch () {
            if(self.searchText) {
            return $http.get(contextUrl() + "/wn/" + self.searchText)
                .then(function(result) {
                    self.items = result.data.map(function(s) {
                        var ili = s.match(".* <([icp]\\d+)>")[1];
                        var defn = s.match(".*: (.*) <.*>")[1];
                        var pos = s.match(".* \\((.)\\):.*")[1];
                        return {"display": s, "ili": ili, "definition": defn, "pos": pos};
                    });
                    return self.items;
                });
            } else {
                clearItem();
            }
        }

        self.selectItem = function(item) {
            if(item) {
                $scope.$parent.sense.synset.id = item.ili;
                $scope.$parent.sense.synset.definition = item.definition;
                $scope.$parent.sense.external = true;
                $scope.$parent.sense.synset.pos = item.pos;
                self.items = [];
            } 
        }

        self.clearItem = function() {
            $scope.$parent.sense.synset = {
                "id": "",
                "pos": $scope.$parent.sense.synset.pos,
                "definition": ""
            };
            $scope.$parent.sense.external = false;
            self.searchText = "";
        }
});

