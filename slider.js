class RangeSlider extends HTMLElement {
  // Called when the slider is about to be added to the DOM
  connectedCallback() {
    // Create new input element
    const input = document.createElement('input');

    // Add <input> to the DOM inside the <range-slider>
    this.appendChild(input);

    // The first two options in the config are the values we set in Elm!
    const jsr = new JSR(input, {
      max: this.max,
      values: [this.val],
      sliders: 1,
      grid: false,
    });

    const rangeSliderNode = this;

    // Whenever the user drags the slider around, the <range-slider> element
    // will emit a "slide" event containing the new value they've chosen

    // Listen for update events from the jsr object
    jsr.addEventListener('update', (elem, value) => {
      // Create new slide event and store value inside of it
      const event = new CustomEvent('slide', {
        detail: { userSlidTo: value },
      });

      // Dispatch event from <range-slider>
      rangeSliderNode.dispatchEvent(event);
    });
  }
}

// Register <range-slider> in the browser
window.customElements.define('range-slider', RangeSlider);
