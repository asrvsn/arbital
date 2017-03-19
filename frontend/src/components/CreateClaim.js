import React, { Component } from 'react'

import FlatButton from 'material-ui/FlatButton'
import Dialog from 'material-ui/Dialog'
import TextField from 'material-ui/TextField'
import RaisedButton from 'material-ui/RaisedButton';

import backend from '../util/backend'

import CreateArgument from './CreateArgument'

const styles = {
  addArgument: {
    margin: 12
  }
}

class CreateClaim extends Component {

  constructor(props) {
    super(props)
    this.state = {
      childOpen: false,
      submitted: false,
      args: [],
      claim: null
    }
  }

  render() {
    const { open } = this.props
    const { childOpen, submitted, claim, args } = this.state

    const actions = [
      <FlatButton
        label="Cancel"
        primary={true}
        onTouchTap={() => this.close()}
      />,
      <FlatButton
        label={submitted ? "Already submitted" : "Submit"}
        primary={true}
        keyboardFocused={true}
        onTouchTap={() => this.submit()}
        disabled={! submitted}
      />,
    ]

    return (
      <Dialog
        title="New claim"
        actions={actions}
        modal={false}
        open={open}
        onRequestClose={() => this.close()}
      >
        <TextField
          hintText="Enter claim text"
          ref={elem => this.claimTextElem = elem}
        />
        <br />
        <RaisedButton
          label="Add argument"
          style={styles.addArgument}
          onTouchTap={() => this.openChild()}
        />
        { childOpen && claim &&
          <CreateArgument
            claimId={claim.id}
            onRequestClose={(argument) => this.closeChild(argument)}
          />
        }
      </Dialog>
    )
  }

  getClaimCreator() {
    debugger
    const { args } = this.state
    const text = this.claimTextElem.value
    return {text, args}
  }

  close() {
    const { onRequestClose } = this.props
    const { submitted } = this.state
    if (submitted) {
      onRequestClose(this.state.claim)
    } else {
      this.submit(claim => onRequestClose(claim))
    }
  }

  submit(cb = (claim) => {}) {
    const claimCreator = this.getClaimCreator()
    backend
      .post('/claims/create', claimCreator, (err, response, body) => {
        if (err !== null) {
          throw err
        } else {
          if (response.statusCode == 200) {
            const claim = JSON.parse(body)
            this.setState({submitted: true, claim})
            cb(claim)
          } else {
            throw response.statusMessage
          }
        }
      })
  }

  openChild() {
    this.submit(() => {
      this.setState({childOpen: true})
    })
  }

  closeChild(argument) {
    const { args } = this.state
    if (argument !== null) {
      args.push(argument)
    }
    this.setState({
      childOpen: false,
      args
    })
  }
}

export default CreateClaim
