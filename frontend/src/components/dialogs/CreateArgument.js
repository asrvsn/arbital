import React, { Component } from 'react'
import { connect } from 'react-redux'

import Dialog from 'material-ui/Dialog'
import ContentAddBox from 'material-ui/svg-icons/content/add-box';
import Chip from 'material-ui/Chip';
import FlatButton from 'material-ui/FlatButton';
import TextField from 'material-ui/TextField';
import DropDownMenu from 'material-ui/DropDownMenu';
import MenuItem from 'material-ui/MenuItem';
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

import { pushDialog } from '../../actions'
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
      arg: null,
      argText: "",
      isFor: true,
      dynLinkedClaim: null,
      submitEnabled: false,
      openChild: null,
      childAnchorEl: null
    }
  }

  componentWillReceiveProps(nextProps) {
    const { pushState } = nextProps
    if ((!! pushState) && (pushState.action == 'POP') && pushState.dialogType == 'CREATE_CLAIM') {
      const { props: { createdClaim } } = pushState
      const { openChild, supportingClaims } = this.state
      switch(openChild) {
        case 'linkToClaim': {
          this.setState({
            dynLinkedClaim: createdClaim,
            openChild: null
          })
        }
        case 'addSupportingClaim': {
          const claimExists = supportingClaims.some(c => c.id == createdClaim.id)
          if (! claimExists) {
            supportingClaims.push(createdClaim)
            this.setState({
              supportingClaims,
              openChild: null
            })
          }
        }
      }
    }
  }

  componentDidMount() {
    const { linkedClaim } = this.props
    this.setState({ dynLinkedClaim: linkedClaim })
  }

  render() {
    const {
      openChild,
      supportingClaims,
      arg,
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
                ? `${dynLinkedClaim.text.substr(0,20)}...`
                : 'No claim linked'
              }
            </Chip>
          </div>
          <div style={styles.rightButton}>
            <RaisedButton
              label="Link to claim"
              labelPosition="before"
              primary={true}
              icon={<ContentAddBox />}
              onTouchTap={e => this.openLinkToClaim(e)}
              fullWidth={true}
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
              labelPosition="before"
              backgroundColor="green"
              icon={<ContentAddBox />}
              onTouchTap={e => this.openAddSupportingClaim(e)}
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

  closeChild() {
    this.setState({openChild: null})
  }

  toggleIsFor() {
    this.setState({isFor: ! this.state.isFor})
  }

  openCreateClaimDialog() {
    this.setState({openChild: null})
    const { dispatch } = this.props
    dispatch(pushDialog({
      dialogType: 'CREATE_CLAIM',
      props: {}
    }))
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
            this.setState({arg: body})
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
    if (props.linkedClaimId !== undefined) {
      return {
        claim: {
          path: '/claims/' + linkedClaimId,
          mapResponseToProps: (resp) => ({linkedClaim: resp})
        }
      }
    } else {
        return {}
    }
  }
)
