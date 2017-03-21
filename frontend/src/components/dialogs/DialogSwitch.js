import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import CreateClaim from './CreateClaim'
import CreateArgument from './CreateArgument'

import { popDialog } from '../../actions'

class DialogSwitch extends Component {

  render() {
    const { pushState, dispatch } = this.props
    const { dialogType, props } = pushState

    console.warn('DialogSwitch pushState', pushState)

    switch(dialogType) {
      case 'CREATE_CLAIM': {
        const allProps = Object.assign(
          props,
          {
            onRequestClose: (claim) => dispatch(
              popDialog({
                dialogType,
                props: {
                  createdClaim: claim
                }
              })
            )
          }
        )
        return <CreateClaim {...allProps} />
      }
      case 'CREATE_ARGUMENT': {
        const allProps = Object.assign(
          props,
          {
            onRequestClose: (arg) => dispatch(
              popDialog({
                dialogType,
                props: {
                  createdArgument: arg
                }
              })
            )
          }
        )
        return <CreateArgument {...allProps} />
      }
      default: {
        throw 'DialogSwitch: got invalid dialogType'
      }
    }
  }
}

export default connect(
  null,
  (dispatch) => ({ dispatch })
)(DialogSwitch)
