import React, { Component, PropTypes } from 'react'

const StackedHOC = (ChildComponent) => {

  class StackedComponent extends Component {
    constructor(props) {
      console.warn('StackedComponent constructor')
      super(props)
      this.state = {
        pushStateStack: []
      }
    }

    componentWillReceiveProps(nextProps) {
      console.warn("StackedHOC componentWillReceiveProps", this.state)
      const { pushState } = nextProps
      if (!! pushState) {
        switch(pushState.action) {
          case 'PUSH': {
            this.pushStack(pushState)
            return
          }
          case 'POP': {
            this.popStack(pushState)
            return
          }
        }
      }
    }

    render() {
      const { pushStateStack } = this.state
      console.warn('StackedHOC render: ', pushStateStack)
      return this.renderComponentStack(pushStateStack)
    }

    renderComponentStack(pushStateStack) {
      if (pushStateStack.length > 0) {
        const pushState = pushStateStack.shift()
        const childProps = Object.assign({ pushState }, this.props)
        return (
          <ChildComponent {...childProps}>
            { this.renderComponentStack(pushStateStack) }
          </ChildComponent>
        )
      } else {
        return <noscript />
      }
    }

    getChildProps(pushState) {
      return Object.assign({ pushState }, this.props)
    }

    pushStack(pushState) {
      const { pushStateStack } = this.state
      pushStateStack.push(pushState)
      console.warn('pushStack', pushStateStack)
      this.setState({ pushStateStack })
    }

    popStack(pushState) {
      const { pushStateStack } = this.state
      pushStateStack.pop()
      if (pushStateStack.length > 0) {
        const lastIdx = pushStateStack.length - 1
        pushStateStack[lastIdx] = pushState
      }
      console.warn('popStack', pushStateStack)
      this.setState({ pushStateStack })
    }
  }

  return StackedComponent
}

export default StackedHOC
