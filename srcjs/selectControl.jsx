import { reactShinyInput } from "reactR";
import Select, { components, createFilter } from "react-select";
import parse from "html-react-parser";
import makeAnimated from "react-select/animated";
import {
  SortableContainer,
  SortableElement,
  SortableHandle,
} from 'react-sortable-hoc';
import { ToastContainer, toast } from 'react-toastify';
import 'react-toastify/dist/ReactToastify.css';

// -------------------------------------------------------------------------- //
function arrayMove(array, from, to) {
  const slicedArray = array.slice();
  slicedArray.splice(
    to < 0 ? array.length + to : to,
    0,
    slicedArray.splice(from, 1)[0]
  );
  return slicedArray;
}

const SortableMultiValue = SortableElement(
  (props) => {
    const onMouseDown = (e) => {
      e.preventDefault();
      e.stopPropagation();
    };
    const innerProps = { ...props.innerProps, onMouseDown };
    return <components.MultiValue {...props} innerProps={innerProps} />;
  }
);

const SortableMultiValueLabel = SortableHandle(
  (props) => <components.MultiValueLabel {...props} />
);

const SortableSelect = SortableContainer(Select)

// -------------------------------------------------------------------------- //
const isnonnullobject = (x) => {
  return x !== null && typeof x === "object";
};

const isNotEmpty = (obj) => {
  return Object.keys(obj).length !== 0;
};

const isHTML = (x) => {
  return isnonnullobject(x) && x.hasOwnProperty("__html");
};

function unescapeHtml(html) {
  let el = document.createElement("div");
  return html.replace(/\&[#0-9a-z]+;/gi, function (enc) {
    el.innerHTML = enc;
    return el.innerText;
  });
}

const isKaTeX = (x) => {
  return isnonnullobject(x) && x.hasOwnProperty("__katex");
};

function formatOptionGroup(data, htmlGroups) {
  for (let i = 0; i < data.length; i++) {
    let toparse = htmlGroups[i];
    if(isKaTeX(toparse)){
      data[i].label = parse(katex.renderToString(decodeURI(toparse.__katex)));
    }else{
      data[i].label = parse(unescapeHtml(decodeURI(toparse)));
    }
  }
}

const f = (list) => {
  const keys = Object.keys(list);
  if (keys.length === 3) {
    return (state) =>
      state.isSelected
        ? list["selected"]
        : state.isFocused
          ? list["focused"]
          : list["otherwise"];
  }
  const key = Object.keys(list)[0];
  if (key === "selected") {
    return (state) => (state.isSelected ? list[key] : list["otherwise"]);
  } else {
    return (state) => (state.isFocused ? list[key] : list["otherwise"]);
  }
};

const getStrings = (option) => {
  const label = option.label;
  if(typeof label === "string"){
    return label;
  }
  if(React.isValidElement(label) && Array.isArray(label.props.children)){
    const strings = label.props.children.filter(x => typeof x === "string");
    return strings.join(" ");
  }
  return "";
};

// -------------------------------------------------------------------------- //
class SelectControl extends React.PureComponent {
  constructor(props) {
    super(props);
    this.handleChange = this.handleChange.bind(this);
    this.handleMenuOpen = this.handleMenuOpen.bind(this);
  }

  state = {
    selectedOption: this.props.value,
    menuIsOpen: false
  };

  handleChange = (selectedOption) => {
    this.setState({ selectedOption: selectedOption });
    if (Array.isArray(selectedOption)) {
      selectedOption = selectedOption.map((x) => x.value);
    } else {
      selectedOption = selectedOption.value;
    }
    this.props.setShinyValue(selectedOption);
    $('[data-toggle="tooltip"]').tooltip("hide");
    setTimeout(function(){$('[data-toggle="tooltip"]').tooltip()});
  };

  handleMenuOpen = () => {
    this.setState({ menuIsOpen: true });
    setTimeout(function(){$('[data-toggle="tooltip"]').tooltip()});
  };

  componentDidMount() {
    $('[data-toggle="tooltip"]').tooltip();
  };

  render() {

    const toggleMenuIsOpen = () => {
      const menuIsOpen = this.state.menuIsOpen;
      this.setState({ menuIsOpen: !menuIsOpen });
    };

    Shiny.addCustomMessageHandler("toggleMenu_" + this.props.shinyId, function(x) {
      toggleMenuIsOpen();
    });

    const updateValue = (value) => {
      console.log(this.props.options);
      let selection;
      if(this.props.isMulti) {
        selection = this.props.options.filter(o => o.value === value);
      } else {
        selection = this.props.options.find(o => o.value === value[0]);
      }
      console.log(selection);
      const that = this;
      setTimeout(function() {
        that.handleChange(selection);
      }, 1000);
      // this.setState({ selectedOption: selection });
      console.log(this.state.selectedOption);
    };
  
    Shiny.addCustomMessageHandler("updateValue_" + this.props.shinyId, function(x) {
      updateValue(x);
    });

    const filterConfig = createFilter(
      $.extend(this.props.filterConfig, {stringify: getStrings})
    );

    let obj = {};
    let optionsStyles = {};
    if (isNotEmpty(this.props.optionsStyles)) {
      optionsStyles = this.props.optionsStyles;
      for (let property in optionsStyles) {
        if (typeof optionsStyles[property] === "object") {
          obj[property] = f(optionsStyles[property]);
        } else {
          obj[property] = optionsStyles[property];
        }
      }
    }
    const controlStyles = this.props.controlStyles;
    const multiValueStyles = this.props.multiValueStyles;
    const multiValueLabelStyles = this.props.multiValueLabelStyles;
    const multiValueRemoveStyles = this.props.multiValueRemoveStyles;

    let animatedComponents = null;
    if (this.props.animated) {
      animatedComponents = makeAnimated();
    }

    const customStyles = {
      option: (provided, state) => {
        let obj1 = { ...provided };
        let obj2 = {};
        for (let property in optionsStyles) {
          if (typeof optionsStyles[property] === "object") {
            obj2[property] = obj[property](state);
          } else {
            obj2[property] = obj[property];
          }
        }
        return $.extend(obj1, obj2);
      },
      control: (provided) => ({
        ...provided,
        ...controlStyles
      }),
      multiValue: (provided, { data }) => {
        return {
          ...provided,
          ...multiValueStyles
        };
      },
      multiValueLabel: (provided, { data }) => ({
        ...provided,
        ...multiValueLabelStyles
      }),
      multiValueRemove: (provided, { data }) => ({
        ...provided,
        ...multiValueRemoveStyles
      })
    };

    let labelTag = null;
    if (this.props.label) {
      labelTag = parse(`<label>${this.props.label}</label>`);
    }

    if (this.props.grouped) { // case of grouped options -------------------- //
      const groupStyles = {
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between"
      };

      const groupBadgeStyles = {
        backgroundColor: "#EBECF0",
        borderRadius: "2em",
        color: "#172B4D",
        display: "inline-block",
        fontSize: 12,
        fontWeight: "normal",
        lineHeight: "1",
        minWidth: 1,
        padding: "0.16666666666667em 0.5em",
        textAlign: "center"
      };

      let groupLength = (data) => null;
      if (this.props.displayGroupSizes) {
        groupLength = (data) => data.options.length;
      }

      const formatGroupLabel = (data) => (
        <div style={groupStyles}>
          <span>{data.label}</span>
          <span style={groupBadgeStyles}>{groupLength(data)}</span>
        </div>
      );

      if (this.props.sortable) { // case of sortable items ------------------ //

        const onSortEnd = ({ oldIndex, newIndex }) => {
          let newValue = arrayMove(this.state.selectedOption, oldIndex, newIndex);
          this.setState({ selectedOption: newValue });
          if (Array.isArray(newValue)) {
            newValue = newValue.map((x) => x.value);
          } else {
            newValue = newValue.value;
          }
          this.props.setShinyValue(newValue);
        };

        let componentsProp = {
          MultiValue: SortableMultiValue,
          MultiValueLabel: SortableMultiValueLabel
        };
        if (this.props.animated) {
          componentsProp.MultiValueRemove = animatedComponents.MultiValueRemove;
        }

        return (
          <div className={this.props.containerClass}>
            <ToastContainer />
            {labelTag}
            <SortableSelect
              useDragHandle
              // react-sortable-hoc props:
              axis="xy"
              onSortEnd={onSortEnd}
              distance={4}
              // small fix for https://github.com/clauderic/react-sortable-hoc/pull/352:
              getHelperDimensions={({ node }) => node.getBoundingClientRect()}
              // react-select props:
              styles={customStyles}
              isMulti
              options={this.props.options}
              formatGroupLabel={formatGroupLabel}
              defaultValue={this.props.value}
              value={this.state.selectedOption}
              onChange={this.handleChange}
              components={componentsProp}
              closeMenuOnSelect={this.props.closeMenuOnSelect}
              filterOption={filterConfig}
              menuIsOpen={this.state.menuIsOpen}
              onMenuOpen={this.handleMenuOpen}
              onMenuClose={() => (this.setState({ menuIsOpen: false }))}
            />
          </div>
        );

      } else { // not sortable items ---------------------------------------- //

        return (
          <div className={this.props.containerClass}>
            <ToastContainer />
            {labelTag}
            <Select
              styles={customStyles}
              defaultValue={this.props.value}
              value={this.state.selectedOption}
              onChange={this.handleChange}
              options={this.props.options}
              formatGroupLabel={formatGroupLabel}
              components={animatedComponents}
              isMulti={this.props.isMulti}
              closeMenuOnSelect={this.props.closeMenuOnSelect}
              filterOption={filterConfig}
              menuIsOpen={this.state.menuIsOpen}
              onMenuOpen={this.handleMenuOpen}
              onMenuClose={() => (this.setState({ menuIsOpen: false }))}
            />
          </div>
        );
      }

    } else { // no groups --------------------------------------------------- //

      if (this.props.sortable) { // sortable items -------------------------- //

        const onSortEnd = ({ oldIndex, newIndex }) => {
          let newValue = arrayMove(this.state.selectedOption, oldIndex, newIndex);
          this.setState({ selectedOption: newValue });
          if (Array.isArray(newValue)) {
            newValue = newValue.map((x) => x.value);
          } else {
            newValue = newValue.value;
          }
          this.props.setShinyValue(newValue);
        };

        let componentsProp = {
          MultiValue: SortableMultiValue,
          MultiValueLabel: SortableMultiValueLabel
        };
        if (this.props.animated) {
          componentsProp.MultiValueRemove = animatedComponents.MultiValueRemove;
          componentsProp.MultiValueContainer = animatedComponents.MultiValueContainer;
        }

        return (
          <div className={this.props.containerClass}>
            <ToastContainer />
            {labelTag}
            <SortableSelect
              useDragHandle
              // react-sortable-hoc props:
              axis="xy"
              onSortEnd={onSortEnd}
              distance={4}
              // small fix for https://github.com/clauderic/react-sortable-hoc/pull/352:
              getHelperDimensions={({ node }) => node.getBoundingClientRect()}
              // react-select props:
              styles={customStyles}
              isMulti
              options={this.props.options}
              defaultValue={this.props.value}
              value={this.state.selectedOption}
              onChange={this.handleChange}
              components={componentsProp}
              closeMenuOnSelect={this.props.closeMenuOnSelect}
              filterOption={filterConfig}
              menuIsOpen={this.state.menuIsOpen}
              onMenuOpen={this.handleMenuOpen}
              onMenuClose={() => (this.setState({ menuIsOpen: false }))}
            />
          </div>
        );

      } else { // not sortable items ---------------------------------------- //
        return (
          <div className={this.props.containerClass}>
            <ToastContainer />
            {labelTag}
            <Select
              styles={customStyles}
              defaultValue={this.props.value}
              value={this.state.selectedOption}
              onChange={this.handleChange}
              options={this.props.options}
              isMulti={this.props.isMulti}
              components={animatedComponents}
              closeMenuOnSelect={this.props.closeMenuOnSelect}
              filterOption={filterConfig}
              menuIsOpen={this.state.menuIsOpen}
              onMenuOpen={this.handleMenuOpen}
              onMenuClose={() => (this.setState({ menuIsOpen: false }))}
            />
          </div>
        );
      }
    }
  }
}

// -------------------------------------------------------------------------- //
const SelectControlInput = ({ configuration, value, setValue }) => {
  let label = configuration.label;
  if (isHTML(label)) {
    label = unescapeHtml(decodeURI(label.__html));
  }
  const grouped = configuration.grouped;
  let selected = configuration.selected;
  if (!Array.isArray(selected)) {
    selected = [selected];
  }
  let options = configuration.options;
  if (grouped && configuration.htmlGroups) {
    formatOptionGroup(options, configuration.htmlGroups);
  }
  if (configuration.htmlLabels) {
    if (grouped) {
      for (let i = 0; i < options.length; i++) {
        formatOptionGroup(options[i].options, configuration.htmlLabels[i]);
      }
    } else {
      formatOptionGroup(options, configuration.htmlLabels);
    }
  }
  let selections = [];
  for (let i = 0; i < selected.length; i++) {
    let group = grouped ? options[selected[i].group].options : options;
    let valueIndices = selected[i].selected;
    if(Array.isArray(valueIndices)){
      selections = selections.concat(valueIndices.map((j) => (group[j])));
    }else{
      selections.push(group[valueIndices]);
    }
  }

  return (
    <SelectControl
      shinyId={configuration.shinyId}
      grouped={configuration.grouped}
      containerClass={configuration.containerClass}
      label={label}
      options={options}
      value={selections}
      setShinyValue={setValue}
      optionsStyles={configuration.optionsStyles}
      controlStyles={configuration.controlStyles}
      multiValueStyles={configuration.multiValueStyles}
      multiValueLabelStyles={configuration.multiValueLabelStyles}
      multiValueRemoveStyles={configuration.multiValueRemoveStyles}
      isMulti={configuration.isMulti}
      sortable={configuration.sortable}
      animated={configuration.animated}
      displayGroupSizes={configuration.displayGroupSizes}
      closeMenuOnSelect={configuration.closeMenuOnSelect}
      filterConfig={configuration.filterConfig}
    />
  );
};

// -------------------------------------------------------------------------- //
reactShinyInput(
  ".selectControl",
  "shinySelect.selectControl",
  SelectControlInput,
  {
    receiveMessage: function(el, data){
      let toastify = false;
      let config = this.getInputConfiguration(el);
      if(!config.isMulti && Array.isArray(data.value) && data.value.length > 1){
        toastify = true;
      }
      config = $.extend(config, data.config);
      this.unsubscribe(el);
      this.setInputConfiguration(el, config);
      if(data.value){
        this.setValue(el, data.value);
      }
      this.render(el);
      if(toastify){
        toast.info("Switching to multiple mode", {
          position: "top-center",
          autoClose: 9000,
          hideProgressBar: false,
          closeOnClick: true,
          pauseOnHover: true,
          draggable: true,
          progress: undefined,
          style: {backgroundColor: "black", color: "white", fontFamily: "cursive"}
        });
      }
    }
  }
);
