$("div.main-container").append('<div class="multiverse-navbar"><p id="multiverse-params"></p></div>')


/*
  choices is an Object where the keys are the parameter names and the value is a list of its option names
  
    choices = {
      param-1 : [option1_param1, option2_param1, ...],
      param-2 : [option1_param2, option2_param2, ...],
      ...
    }
  
  combos is a list of Objects. It contains all combinations of option names that are available in the dataset
    
    combos = [
      {
        param-1 : option1_param1,
        param-2 : option1_param2,
        ...
      },
      {
        param-1 : option2_param1,
        param-2 : option2_param2,
        ...
      },
      ...
    ]
*/
let choices, combos;
let tangleObj;

function sortOnKeys(dict) {
    let sorted = Object.keys(dict);
    sorted.sort();

    let tempDict = {};
    for(let i = 0; i < sorted.length; i++) {
        tempDict[sorted[i]] = dict[sorted[i]];
    }

    return tempDict;
}

/*
  comboExists checks if curr is in combos

  PARAMETERS:
  curr:Object
  
  RETURNS: curr is in combos ? true : false
*/
// since combos is a global variable, i feel like it's not needed in the params
let comboExists = curr => {
  // sort current specication based on parameter names:
  sortedCurr = sortOnKeys(curr)
  for (let combo of combos) {
    if (JSON.stringify(sortedCurr) == JSON.stringify(sortOnKeys(combo))) return true;
  }
  return false;
}

/*
  Changes the currently set options of each parameter to what is specified in the
  input `spec`, if `spec` is a valid combination.
  
  PARAMETERS:
  spec: Object of the form {'param1_name': 'option_name', 'param2_name': 'option_name', ... } (the values must be a string);
  all parameters declared in the multiverse must be specified.
*/
const setActiveSpecification = spec => {
  if (comboExists(spec)) {
    /*
      Finds the index of the desired option as found in choices, and updates each
      variable with the name a parameter in the initialized tangle object, which
      then calls its update function.
    */
    let comboIdx = {};
    for (let param of Object.keys(choices)) {
      comboIdx[param] = choices[param].findIndex(e => e==spec[param]);
      tangleObj.setValue(param, spec[param]);
    }
    
    /*
      Implicit assumption that all tangle widgets relate to each other with the
      same parameters, i.e. all elements with the ".Iterate" class are a Tangle
      widget that have the same set of parameters and associated options
      
      Updates each tangle widget with its accordingly changed values.
    */
    for (let mv of $('.Iterate')) {
      let param = mv.getAttribute("data-var");
      mv.setAttribute("data-i", comboIdx[param]);
      mv.textContent = param + ": " + spec[param];
    }
  } else {
    console.log('specification does not exists')
  }
}

Tangle.classes.Iterate = {
  /*
    element - the HTML element that was clicked
    options - the data contained in the HTML element itself
    tangle - used to get this.<variable-name> in the Tangle instance as initialized in setup()
    variable - the parameter name that was clicked
  */
  initialize: function(element, options, tangle, variable) {
    element.addEvent("click", (event) => {
      let keys = Object.keys(choices);
      
      // curr takes the current combination of option names of each parameter
      let curr = {}
      for (let key of keys) {
        curr[key] = tangle.getValue(key)
      }
      
      // this is used instead of options.i in order for setActiveSpecification to work
      let i = element.getAttribute("data-i");
      
      // gets next available option in current param given the other params' options
      for (let it = 0; it < choices[variable].length; it++) {
        if (++i === choices[variable].length)
          i = 0;
        curr[variable] = choices[variable][i];
        if (comboExists(curr)) break;
      }
      element.setAttribute("data-i", i);
      tangle.setValue(variable, choices[variable][i]);
    })
  },
  update: function(element, value) {
    
    element.textContent = element.getAttribute("data-var") + ": " +value;
  }
}

function setup() {
  let t = new Tangle(
    document,
    {
      initialize: function() {
        const pre = $("pre.multiverse").get();
        this.choices = {};
        this.combos = [];
        for (let e of pre) {
          let paramOption = {};
          for (let className of e.classList) {
            if (className.includes("---")) {
              let split = className.split("---");
              paramOption[split[0]] = split[1];
              if (split[0] in this.choices) this.choices[split[0]].add(split[1]);
              else this.choices[split[0]] = new Set([split[1]]);
            }
          }
          this.combos.push(paramOption);
        }
        //this.combos = new Set(this.combos);
        this.keys = Object.keys(this.choices);
        for (let k of this.keys) {
          this.choices[k] = [...this.choices[k]];
          this[k] = this.choices[k][0];
        }
        choices = this.choices;
        combos  = this.combos;
      },
      update: function() {
        let universe = {}
        for (let k of this.keys) {
          universe[k] = this[k];
        }
        /*
          When dealing with possible figures following each "pre" tag, the
          following code, specifically referring to the .each functions, assumes
          that there exists another "pre" tag (regardless if it is displayed or
          not) after the "pre" tag that is to be displayed. As a visual,
          
          <pre class="r multiverse ...">...</pre> <-- to be displayed
          ...                                     <-- warnings / figs that may (not) exist
          <pre class="r multiverse ..."...>...</pre>                      <-- next multiverse "pre" tag
        */
        // TODO: change to find succeeding <p> tags
        let preMV = $("pre.multiverse");
        // console.log(preMV);
        preMV.each(
          (i,e) => { $(e).nextUntil('pre.multiverse').hide(); }
        );
        let pre = $("." + Object.entries(universe).map(d => d.join('---')).join('.'));
        pre.show();
        pre.each(
          (i,e) => { $(e).nextUntil('pre.multiverse').show(); }
        );
      }
    }
  );
  tangleObj = t;
}

// given a specification of the multiverse
// and a list of parameters
// return a list of classess to be added
function getClasses(spec, p) {
  let classes = Object.entries(spec).filter(i => !(p.includes(i[0])))
  return classes.map(d => d[1].map(x => d[0] + "---" + x)).flat()
}

const load = () => {
  const pre = $("pre.multiverse").get();
  let multiverse_spec = {}
  for (let e of pre) {
    let universe = [...e.classList].filter(i => i.includes("---")).map(d => d.split("---"));
    universe.forEach(d => {
      if (Object.keys(multiverse_spec).includes(d[0])) {
        if (!multiverse_spec[d[0]].includes(d[1])) {
          multiverse_spec[d[0]].push(d[1])
        }
      } else {
        multiverse_spec[d[0]] = [d[1]]
      }
    })
  }
  
  let parameters = Object.keys(multiverse_spec);
  // creates a tangle widget for each parameter in the declared multiverse
  for (let i of parameters) {
      $("p#multiverse-params").append(`<span class="Iterate TKSwitch" data-var=${i} data-i=0></span><br>`)
  }
  // replaces <mv param="[param name here]"></mv> with the tangle widget
  for (let e of $("mv")) {
      e.replaceWith($(`<span class="Iterate TKSwitch" data-var=${e.getAttribute("param")} data-i=0></span>`)[0])
  }
  // adds classes for <pre> tags where parameters are missings
  for (let e of pre) {
    let current_params = [...e.classList].filter(i => i.includes("---")).map(d => d.split("---")[0]);
    // first get the elements where we need to add classes
    if (current_params.length != parameters.length) {
      // for each of the elements, add classes for each p---o combinations of the parameters not included
      let to_add = getClasses(multiverse_spec, current_params)
      e.classList.add(...to_add)
    }
  }
  
  setup();
    
  $("div.main-container").css("margin-top", 40 + $("div.multiverse-navbar").height());
}
  
window.onload = load;