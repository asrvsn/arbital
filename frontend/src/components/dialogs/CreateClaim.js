import React, { Component } from 'react'
import { connect } from 'react-redux'

import FlatButton from 'material-ui/FlatButton'
import Dialog from 'material-ui/Dialog'
import TextField from 'material-ui/TextField'
import RaisedButton from 'material-ui/RaisedButton';

import backend from '../../util/backend'
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
      text: ""
    }
  }

  render() {
    const { text } = this.state

    const actions = [
      <FlatButton
        label="Cancel"
        primary={true}
        onTouchTap={() => this.close(null)}
      />,
      <FlatButton
        label="Submit"
        primary={true}
        onTouchTap={() => this.submit(claim => this.close(claim))}
      />,
    ]

    return (
      <Dialog
        title="New claim"
        actions={actions}
        modal={false}
        open={true}
        onRequestClose={() => this.close(null)}
      >

        <TextField
          hintText="Enter claim text"
          onChange={e => this.onChange(e)}
          fullWidth={true}
          multiLine={true}
          value={text}
        />

      </Dialog>
    )
  }

  onChange(e) {
    this.setState({text: e.target.value})
  }

  getClaimCreator() {
    const { text } = this.state
    return {text, args: []}
  }

  close(claim) {
    this.props.onRequestClose(claim)
  }

  submit(cb = (claim) => {}) {
    const { session } = this.props
    const claimCreator = this.getClaimCreator()
    debugger
    const url = '/claims/create'
    backend
      .authenticate(session.id)
      .post(url, claimCreator, (err, response, body) => {
        if (err !== null) {
          throw err
        } else {
          if (response.statusCode == 200) {
            cb(body)
          } else {
            throw response.statusMessage
          }
        }
      })
  }
}

export default AuthenticatedHOC(
  connect(
    null,
    (dispatch) => ({ dispatch })
  )(CreateClaim)
)
