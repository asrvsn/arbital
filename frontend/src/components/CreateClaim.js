import React, { Component } from 'react'

import FlatButton from 'material-ui/FlatButton'
import Dialog from 'material-ui/Dialog'
import TextField from 'material-ui/TextField'

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
      open: true,
      childOpen: false
    }
  }

  render() {
    const { open, childOpen } = this.state
    const claimId = undefined // TODO get from props.location

    const actions = [
      <FlatButton
        label="Cancel"
        primary={true}
        onTouchTap={() => this.close()}
      />,
      <FlatButton
        label="Submit"
        primary={true}
        keyboardFocused={true}
        onTouchTap={() => this.submit()}
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
        />
        <br />
        <RaisedButton
          label="Add argument"
          style={styles.addArgument}
          onTouchTap={() => this.openChild()}
        />
        { childOpen &&
          <CreateArgument claimId={claimId} />
        }
      </Dialog>
    )
  }

  close() {
    this.setState({open: false})
  }

  submit() {
    // TODO submit claim to backend
  }

  openChild() {
    this.setState({childOpen: true})
  }
}

export default CreateClaim
