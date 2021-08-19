class RangeSlider extends HTMLElement {
  // Called when the slider is about to be added to the DOM
  connectedCallback() {
    // Create new input element
    const input = document.createElement("input");

    // Add <input> to the DOM inside the <range-slider>
    this.appendChild(input);

    // The first two options in the config are the values we set in Elm!
    const jsr = new JSR(input, {
      max: this.max,
      values: [this.val],
      sliders: 1,
      grid: false
    })
  }
}

// Register <range-slider> in the browser
window.customElements.define("range-slider", RangeSlider);