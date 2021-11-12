import { reactShinyInput } from "reactR";
import Select, { components } from "react-select";
import parse from "html-react-parser";
import makeAnimated from "react-select/animated";
import {
  SortableContainer,
  SortableContainerProps,
  SortableElement,
  SortEndHandler,
  SortableHandle,
} from 'react-sortable-hoc';


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

function formatOptionGroup(data, htmlGroups) {
  for (let i = 0; i < data.length; i++) {
    data[i].label = parse(unescapeHtml(decodeURI(htmlGroups[i])));
  }
}

function formatKaTeX(data) {
  for (let i = 0; i < data.length; i++) {
    data[i].label = parse(katex.renderToString(data[i].label));
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

// -------------------------------------------------------------------------- //
class SelectControl extends React.PureComponent {
  constructor(props) {
    super(props);
    this.handleChange = this.handleChange.bind(this);
  }

  state = {
    selectedOption: this.props.value
  };

  handleChange = (selectedOption) => {
    this.setState({ selectedOption: selectedOption });
    console.log(`Option selected:`, selectedOption);
    console.log(this.state.selectedOption);
    if (Array.isArray(selectedOption)) {
      selectedOption = selectedOption.map((x) => x.value);
    } else {
      selectedOption = selectedOption.value;
    }
    this.props.setShinyValue(selectedOption);
  };

  render() {
    const { selectedOption } = this.state;

    let obj = {};
    let styles;
    if (this.props.styles) {
      styles = this.props.styles;
      for (let property in styles) {
        if (typeof styles[property] === "object") {
          obj[property] = f(styles[property]);
        } else {
          obj[property] = styles[property];
        }
      }
    }
    console.log("obj", obj);

    let animatedComponents = null;
    if (this.props.animated) {
      animatedComponents = makeAnimated();
    }
    console.log("animatedComponents", animatedComponents);

    const customStyles = {
      option: (provided, state) => {
        let obj1 = { ...provided };
        let obj2 = {};
        for (let property in styles) {
          if (typeof styles[property] === "object") {
            obj2[property] = obj[property](state);
          } else {
            obj2[property] = obj[property];
          }
        }
        return $.extend(obj1, obj2);
      },
      control: (provided) => ({
        ...provided,
        marginTop: "0"
      })
    };

    let labelTag = null;
    if (this.props.label) {
      labelTag = parse(`<label>${this.props.label}</label>`);
    }

    if (this.props.grouped) {
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

      return (
        <div className={this.props.containerClass}>
          {labelTag}
          <Select
            styles={customStyles}
            defaultValue={this.props.value}
            onChange={this.handleChange}
            options={this.props.options}
            formatGroupLabel={formatGroupLabel}
            components={animatedComponents}
            isMulti={this.props.isMulti}
            closeMenuOnSelect={this.props.closeMenuOnSelect}
          />
        </div>
      );
    } else {

      if (this.props.sortable) {

        //        const [selected, setSelected] = React.useState(this.props.value);

        // const onChange = (selectedOptions) => {
        //   setSelected(selectedOptions);
        //   if (Array.isArray(selectedOptions)) {
        //     selectedOptions = selectedOptions.map((x) => x.value);
        //   } else {
        //     selectedOptions = selectedOptions.value;
        //   }
        //   this.props.setShinyValue(selectedOptions);
        // }

        const onSortEnd = ({ oldIndex, newIndex }) => {
          let newValue = arrayMove(this.state.selectedOption, oldIndex, newIndex);
          console.log(
            'Values sorted:',
            newValue.map((i) => i.value)
          );
          //         setSelected(newValue);
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
        if(this.props.animated){
          componentsProp.MultiValueRemove = animatedComponents.MultiValueRemove;
        }

        return (
          <div className={this.props.containerClass}>
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
            />
          </div>
        );

      } else {
        return (
          <div className={this.props.containerClass}>
            {labelTag}
            <Select
              styles={customStyles}
              defaultValue={this.props.value}
              onChange={this.handleChange}
              options={this.props.options}
              isMulti={this.props.isMulti}
              components={animatedComponents}
              closeMenuOnSelect={this.props.closeMenuOnSelect}
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
  //const Tag = grouped ? GroupedSelectControl : SelectControl;
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
      for (let i = 0; i < options.length; i++) {
        formatOptionGroup(options, configuration.htmlLabels);
      }
    }
  }
  //formatKaTeX(options)
  let selections = [];
  for (let i = 0; i < selected.length; i++) {
    let group = grouped ? options[selected[i].group].options : options;
    let valueIndices = selected[i].selected;
    selections = selections.concat(group[valueIndices]);
    //selections.push(group[valueIndices]);
  }
  return (
    <SelectControl
      grouped={configuration.grouped}
      containerClass={configuration.containerClass}
      label={label}
      options={options}
      value={selections}
      setShinyValue={setValue}
      styles={configuration.styles}
      isMulti={configuration.isMulti}
      sortable={configuration.sortable}
      animated={configuration.animated}
      displayGroupSizes={configuration.displayGroupSizes}
      closeMenuOnSelect={configuration.closeMenuOnSelect}
    />
  );
};

// -------------------------------------------------------------------------- //
reactShinyInput(
  ".selectControl",
  "shinySelect.selectControl",
  SelectControlInput
);
