import React, { Component, PropTypes } from 'react'

const StackedHOC = (ChildComponent) => {

  class StackedComponent extends Component {
    constructor(props) {
      super(props)
      this.state = {
        pushStateStack: []
      }
    }

    componentWillReceiveProps(nextProps) {
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
      const childProps = Object.assign({ pushState }, this.props)
      delete childProps.pushConfig
      delete childProps.popConfig
      return childProps
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
      const lastIdx = pushStateStack.length - 1
      pushStateStack[lastIdx] = pushState
      console.warn('popStack', pushStateStack)
      this.setState({ pushStateStack })
    }
  }

  return StackedComponent
}

export default StackedHOC
