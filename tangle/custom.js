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

/*
  comboExists checks if curr is in combos

  PARAMETERS:
  curr:Object
  combos:Array[Object]
  
  RETURNS: curr in combos ? true : false
*/
let comboExists = (curr, combos) => {
  curr = JSON.stringify(curr);
  for (let combo of combos) {
    if (curr == JSON.stringify(combo)) return true;
  }
  return false;
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
      choices = tangle.getValue("choices");
      combos = tangle.getValue("combos");
      let keys = Object.keys(choices);
      
      // curr takes the current combination of option names of each parameter
      let curr = {}
      for (let key of keys) {
        curr[key] = tangle.getValue(key)
      }
      
      // in each iteration, curr[variable] is set to the (options.i+1)th element in choices[variable]
      // - if option+1 > the length of choices[variable], then choose the 0th element.
      // after this change, if curr is in combos, then done. else, next iteration.
      // `it` is set to choices[variable].length because if it iterates through the whole list with no change, then iterations should stop
      let it = choices[variable].length;
      do {
        if (options.i === choices[variable].length-1)
          options.i = 0;
        else
          options.i++;
        curr[variable] = choices[variable][options.i];
      } while (!(comboExists(curr, combos)) && it-- > 0);
      
      tangle.setValue(variable, choices[variable][options.i]);
    })
  },
  update: function(element, value) {
    element.textContent = element.getAttribute("data-var") + ": " +value;
  }
}

function setup() {
  // let elem = $("p#multiverse-params").get(0); // probably not gonna be used, but just in case
  
  let t = new Tangle(
    $(document)[0],
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
        this.combos = new Set(this.combos);
        this.keys = Object.keys(this.choices);
        for (let k of this.keys) {
          this.choices[k] = [...this.choices[k]];
          this[k] = this.choices[k][0];
        }
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
          ...                                     <-- figs that may (not) exist
          <pre ...>...</pre>                      <-- next "pre" tag
        */
        // TODO: change to find succeeding <p> tags
        let preMV = $("pre.multiverse");
        preMV.hide();
        preMV.each(
          (i,e) => { $(e).nextUntil('pre').hide(); }
        );
        let pre = $("." + Object.entries(universe).map(d => d.join('---')).join('.'));
        pre.show();
        pre.each(
          (i,e) => { $(e).nextUntil('pre').show(); }
        );
      }
    }
  )
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