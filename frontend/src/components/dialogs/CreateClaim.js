import React, { Component } from 'react'
import { connect } from 'react-redux'

import FlatButton from 'material-ui/FlatButton'
import Dialog from 'material-ui/Dialog'
import TextField from 'material-ui/TextField'
import RaisedButton from 'material-ui/RaisedButton';

import backend from '../../util/backend'
import { pushDialog } from '../../actions'
import AuthenticatedHOC from '../hoc/AuthenticatedHOC'

const styles = {
  addArgument: {
    margin: 12
  }
}

class CreateClaim extends Component {

  constructor(props) {
    super(props)
    this.state = {
      submitted: false,
      linkedArguments: [],
      claim: null
    }
  }

  componentWillReceiveProps(nextProps) {
    const { pushState } = nextProps
    const { linkedArguments } = this.state
    if ((!! pushState) && (pushState.action == 'POP') && (pushState.dialogType == 'CREATE_ARGUMENT')) {
      const { props: { createdArgument } } = pushState
      const argExists = linkedArguments.some((arg) => arg.id == createdArgument.id)
      if (! argExists) {
        linkedArguments.push(createdArgument)
        this.setState({ linkedArguments })
      }
    }
  }

  render() {
    const { submitted, claim, linkedArguments } = this.state

    const actions = [
      <FlatButton
        label="Cancel"
        primary={true}
        onTouchTap={() => this.close(null)}
      />,
      <FlatButton
        label={submitted ? "Already submitted" : "Submit"}
        primary={true}
        onTouchTap={() => this.submit(claim => this.close(claim))}
        disabled={submitted}
      />,
    ]

    return (
      <Dialog
        title="New claim"
        actions={actions}
        modal={false}
        open={true}
        onRequestClose={() => this.close(claim)}
      >

        <TextField
          hintText="Enter claim text"
          ref={elem => this.claimTextElem = elem}
          fullWidth={true}
        />

        <br />

        <RaisedButton
          label="Add argument for claim"
          style={styles.addArgument}
          onTouchTap={() => this.openChild()}
        />

      </Dialog>
    )
  }

  getClaimCreator() {
    const { linkedArguments } = this.state
    const text = this.claimTextElem.input.value
    return {
      text,
      args: linkedArguments.map(arg => arg.id)
    }
  }

  close(claim) {
    this.props.onRequestClose(claim)
  }

  submit(cb = (claim) => {}) {
    const { submitted } = this.state
    if (submitted) {
      cb(this.state.claim)
    } else {
      const { session } = this.props
      const claimCreator = this.getClaimCreator()
      const url = '/claims/create'
      backend
        .authenticate(session.id)
        .post(url, claimCreator, (err, response, body) => {
          if (err !== null) {
            throw err
          } else {
            if (response.statusCode == 200) {
              this.setState({submitted: true, claim: body})
              cb(body)
            } else {
              throw response.statusMessage
            }
          }
        })
    }
  }

  openChild() {
    const { dispatch } = this.props

    this.submit((createdClaim) => {
      dispatch(pushDialog({
        dialogType: 'CREATE_ARGUMENT',
        props: {
          createdClaim
        }
      }))
    })
  }

  closeChild(createdArgument) {
    const {  dispatch } = this.props
    dispatch(popDialog({
      dialogType: 'CREATE_ARGUMENT',
      props: {
        createdArgument
      }
    }))
  }
}

export default AuthenticatedHOC(
  connect(
    null,
    (dispatch) => ({ dispatch })
  )(CreateClaim)
)
