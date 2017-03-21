import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import CreateClaim from './CreateClaim'
import CreateArgument from './CreateArgument'

import { closeDialog } from '../../actions'

class DialogSwitch extends Component {
  render() {
    const { config, dispatch } = this.props
    const { dialogType, payload } = config

    const dialogProps = Object.assign(
      { onRequestClose: (value) => dispatch(closeDialog()) },
      payload
    )

    switch(dialogType) {
      case 'CREATE_CLAIM': {
        return <CreateClaim {...dialogProps} />
      }
      case 'CREATE_ARGUMENT': {
        return <CreateArgument {...dialogProps} />
      }
      case 'NONE': {
        return <noscript />
      }
      default: {
        throw `DialogSwitch got invalid dialogType: ${dialogType}`
      }
    }
  }
}

export default connect(
  ({ currentDialogConfig }) => ({ config: currentDialogConfig }),
  (dispatch) => ({ dispatch })
)(DialogSwitch)
