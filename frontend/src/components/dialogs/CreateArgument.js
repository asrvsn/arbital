import React, { Component } from 'react'
import { connect } from 'react-redux'

import Dialog from 'material-ui/Dialog'
import ContentAddBox from 'material-ui/svg-icons/content/add-box';
import Chip from 'material-ui/Chip';
import FlatButton from 'material-ui/FlatButton';
import TextField from 'material-ui/TextField';
import RaisedButton from 'material-ui/RaisedButton';
import Divider from 'material-ui/Divider';
import {List, ListItem} from 'material-ui/List';
import {Card, CardHeader, CardTitle} from 'material-ui/Card';
import Subheader from 'material-ui/Subheader';
import Popover from 'material-ui/Popover';
import Toggle from 'material-ui/Toggle';

import GetterHOC from '../hoc/GetterHOC'
import ClaimListItem from '../items/ClaimListItem'
import Finder from '../Finder'
import CreateClaim from './CreateClaim'

import backend from '../../util/backend'

const styles = {
  body: {
    paddingLeft: 0,
    paddingRight: 0,
    paddingBottom: 0,
  },
  row: {
    display: 'flex',
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'space-between'
  },
  row1group1: {
    display: 'flex',
    flexDirection: 'row',
    alignItems: 'center',
  },
  title1: {
    paddingRight: 10,
  },
  rightButton: {
    paddingRight: 10,
  },
  input: {
    marginLeft: 16,
    marginRight: 10
  },
  createClaimButton: {
    width: '100%'
  },
  popover: {
    width: 400
  }
}

class CreateArgument extends Component {

  constructor(props) {
    super(props)
    this.state = {
      supportingClaims: [],
      argText: "",
      isFor: true,
      dynLinkedClaim: null,
      submitEnabled: false,
      openChild: null,
      childAnchorEl: null
    }
  }

  componentDidMount() {
    const { linkedClaim, isFor } = this.props
    this.setState({
      dynLinkedClaim: linkedClaim,
      isFor: isFor !== undefined ? isFor : this.state.isFor
    })
  }

  render() {
    const {
      openChild,
      supportingClaims,
      isFor,
      dynLinkedClaim,
      childAnchorEl
    } = this.state

    const actions = [
      <FlatButton
        label="Cancel"
        primary={true}
        onTouchTap={() => this.close(null)}
      />,
      <FlatButton
        label="Submit"
        primary={true}
        onTouchTap={() => this.submit(arg => this.close(arg))}
      />,
    ]

    return (
      <Dialog
        actions={actions}
        modal={false}
        open={true}
        onRequestClose={() => this.close()}
        autoScrollBodyContent={true}
        bodyStyle={styles.body}
      >

        <CardTitle
          title="New Argument"
          subtitle={
            <Toggle
              label={isFor ? 'For' : 'Against'}
              onToggle={() => this.toggleIsFor()}
            />
          }
        />

        <Divider />

        <div style={styles.row}>
          <div style={styles.row1group1}>
            <Subheader style={styles.title1}>
              {`Argument ${isFor ? 'for' : 'against'} `}
            </Subheader>
            <Chip style={styles.chip}>
              {dynLinkedClaim
                ? `${dynLinkedClaim.text.substr(0,40)}...`
                : 'No claim linked'
              }
            </Chip>
          </div>
          <div style={styles.rightButton}>
            <RaisedButton
              label="Link to claim"
              onTouchTap={e => this.openLinkToClaim(e)}
              secondary={true}
            />
            <Popover
              open={openChild === 'linkToClaim'}
              anchorEl={childAnchorEl}
              anchorOrigin={{horizontal: 'right', vertical: 'bottom'}}
              targetOrigin={{horizontal: 'right', vertical: 'top'}}
              onRequestClose={() => this.closeChild()}
              style={styles.popover}
            >
              <Finder
                mode="claims"
                onRequestClose={(claim) => this.closeLinkToClaim(claim)}
              />
            </Popover>
          </div>
        </div>

        <div style={styles.row}>
          <TextField
            style={styles.input}
            hintText="Enter argument summary"
            fullWidth={true}
            multiLine={true}
            onChange={(e, v) => this.setArgText(v)}
          />
        </div>

        <Divider />

        <div style={styles.row}>
          <Subheader>Supporting claims</Subheader>
          <div style={styles.rightButton}>
            <RaisedButton
              label="Add"
              onTouchTap={e => this.openAddSupportingClaim(e)}
              secondary={true}
            />
            <Popover
              open={openChild === 'addSupportingClaim'}
              anchorEl={childAnchorEl}
              anchorOrigin={{horizontal: 'right', vertical: 'bottom'}}
              targetOrigin={{horizontal: 'right', vertical: 'top'}}
              onRequestClose={() => this.closeChild()}
              style={styles.popover}
            >
              <Finder
                mode="claims"
                onRequestClose={(claim) => this.closeAddSupportingClaim(claim)}
              />
              <FlatButton
                label="Create claim"
                style={styles.createClaimButton}
                onTouchTap={e => this.openCreateClaimDialog()}
              />
            </Popover>
          </div>
        </div>

        <List>
          { supportingClaims.map(claim =>
              <ClaimListItem
                key={claim.id}
                claim={claim}
                linksOff={true}
              />
            )
          }
        </List>

        { (openChild === 'createClaimDialog') &&
            <CreateClaim
              onRequestClose={claim => this.closeCreateClaimDialog(claim)}
            />
        }

      </Dialog>
    )
  }

  openLinkToClaim(e) {
    this.setState({
      openChild: 'linkToClaim',
      childAnchorEl: e.currentTarget
    })
  }

  closeLinkToClaim(claim) {
    this.setState({
      dynLinkedClaim: claim,
      openChild: null
    })
  }

  openAddSupportingClaim(e) {
    this.setState({
      openChild: 'addSupportingClaim',
      childAnchorEl: e.currentTarget
    })
  }

  closeAddSupportingClaim(claim) {
    const { supportingClaims } = this.state
    supportingClaims.push(claim)
    this.setState({
      supportingClaims,
      openChild: null
    })
  }

  openCreateClaimDialog() {
    this.setState({openChild: 'createClaimDialog'})
  }

  closeCreateClaimDialog(claim) {
    if (!! claim) {
      this.setState({
        openChild: null,
        supportingClaims: this.state.supportingClaims.concat([claim])
      })
    }
  }

  closeChild() {
    this.setState({openChild: null})
  }

  toggleIsFor() {
    this.setState({isFor: ! this.state.isFor})
  }

  setArgText(v) {
    this.setState({argText: v})
  }

  getArgCreator() {
    const { supportingClaims, argText } = this.state
    return {
      text: argText,
      claims: supportingClaims.map(claim => claim.id)
    }
  }

  close(arg) {
    this.props.onRequestClose(arg)
  }

  submit(cb = (arg) => {}) {
    const argCreator = this.getArgCreator()
    if (argCreator.text === "" || argCreator.claims.length == 0) {
      return
    }

    const { dynLinkedClaim, isFor } = this.state
    const { session } = this.props
    const url = `/claims/${dynLinkedClaim.id}/${isFor ? 'for' : 'against'}`

    backend
      .authenticate(session.id)
      .post(url, argCreator, (err, response, body) => {
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

export default GetterHOC(
  connect(
    null,
    (dispatch) => ({ dispatch })
  )(CreateArgument),
  (props) => {
    const { linkedClaimId } = props
    if (linkedClaimId !== undefined) {
      return {
        claim: {
          path: `/claims/${linkedClaimId}`,
          mapResponseToProps: (resp) => ({linkedClaim: resp})
        }
      }
    } else {
        return {}
    }
  }
)
